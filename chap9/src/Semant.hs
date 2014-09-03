module Semant where

import qualified Data.List as List

import qualified Absyn as A
import qualified Env as E
import qualified Symbol as S
import qualified Types as T
import qualified Translate as TL
import qualified Temp
import qualified DalvikFrame as Frame

type VEnv = S.Table E.EnvEntry
type TEnv = S.Table T.Ty
type Unique = T.Unique

data ExpTy = ExpTy {expr::TL.Exp, ty::T.Ty}
  
data Optype = Arith | Comp | Eq

actual_ty :: Show pos => T.Ty -> pos -> T.Ty
actual_ty typ pos =
  case typ of
    T.NAME s t -> case t of
      Just ty' -> actual_ty ty' pos
      Nothing -> error $ show pos ++ "type not found (in actual_ty): " ++ s
    T.ARRAY ty' u -> T.ARRAY (actual_ty ty' pos) u
    _ -> typ

type_mismatch :: (Show a, Show b, Show c) => a -> b -> c -> t
type_mismatch e a pos =
  error $ show pos ++ "type mismatch: expected " ++ show e ++ ", actual " ++ show a

check_type :: Show pos => T.Ty -> T.Ty -> pos -> Bool
check_type t1 t2 pos =
  let
    t1' = actual_ty t1 pos
    t2' = actual_ty t2 pos
  in
   if t1' /= t2'
   then
     case (t1', t2') of
       (T.RECORD _ _, T.NIL) -> True
       (T.NIL, T.RECORD _ _) -> True
       _ -> type_mismatch t1' t2' pos
   else True
       
must_not_reach :: t
must_not_reach =
  error "fatal: must not reach here"

transProg :: VEnv-> TEnv -> A.Exp -> (T.Ty, [Frame.Frag], Temp.Temp)
            
transProg venv tenv prog = 
  let
    temp = Temp.create
    (mainlevel, temp') = 
      TL.newLevel 
      TL.outermost (Temp.namedLabel "main") [True] temp
    errdest = Temp.namedLabel "_CanNotBreak_"
    (expty, _, frgs, temp'') = transExp venv tenv errdest mainlevel [] temp' prog
    
    -- TODO: unNx should not be public.
    (stm, temp3) = TL.unNx temp'' (expr expty)
    frag = Frame.Proc { Frame.get_body=stm
                      , Frame.get_frame=TL.frame mainlevel}
  in
   (ty expty, frag:frgs, temp3)

transExp :: VEnv-> TEnv -> Temp.Label -> TL.Level -> [Frame.Frag]
            -> Temp.Temp
            -> A.Exp 
            -> (ExpTy, TL.Level, [Frame.Frag], Temp.Temp)
            
transExp venv tenv brkdest =
  let
    trexp :: TL.Level -> [Frame.Frag] -> Temp.Temp
            -> A.Exp 
            -> (ExpTy, TL.Level, [Frame.Frag], Temp.Temp)
            
    trexp level frgs temp A.NilExp = (ExpTy (TL.nilExp) T.NIL, level, frgs, temp)
    
    trexp level frgs temp (A.IntExp i _) = (ExpTy (TL.intExp i) T.INT, level, frgs, temp)
    
    trexp level frgs temp (A.StringExp s _) = 
       (ExpTy{expr=TL.stringExp s, ty=T.STRING}, level, frag:frgs, temp')
    
    trexp level frgs temp A.OpExp{A.oper=oper, A.lhs=lhs, A.rhs=rhs, A.pos=pos} = 
      let
        (ExpTy {expr=e1, ty=lty }, lv', frgs', temp')   = trexp level frgs temp lhs
        (ExpTy {expr=e2, ty=rty }, lv'', frgs'', temp'') = trexp lv' frgs' temp' rhs
        
        classify op = 
          case op of
            A.PlusOp -> Arith
            A.MinusOp -> Arith
            A.TimesOp -> Arith
            A.DivideOp -> Arith
            A.LtOp -> Comp
            A.GtOp -> Comp
            A.LeOp -> Comp
            A.GeOp -> Comp
            A.EqOp -> Eq
            A.NeqOp -> Eq
        
        check_int typ pos' = 
          case typ of
            T.INT -> True
            _     -> error $ show pos' ++ ": integer required."
            
        check_arith = check_int lty pos && check_int rty pos
        
        check_eq = 
          case lty of
            T.INT -> check_type lty rty pos
            T.STRING -> check_type lty rty pos
            T.ARRAY _ _ -> check_type lty rty pos
            T.RECORD _ _ -> check_type lty rty pos
            T.NIL -> check_type lty rty pos
            _ -> error $ show pos ++ "type error for equality operator: " ++ show lty
            
        check_comp =
          case lty of
            T.INT -> check_type lty rty pos
            T.STRING -> check_type lty rty pos
            _ -> error $ show pos ++ "type error for comparison: " ++ show lty
            
        check_result =
          case classify oper of
            Arith -> check_arith 
            Comp -> check_comp
            Eq -> check_eq
            
        trop oper' = 
          case oper' of
            A.PlusOp -> TL.plusOp
            A.MinusOp -> TL.minusOp
            A.TimesOp -> TL.timesOp
            A.DivideOp -> TL.divideOp
            A.LtOp -> TL.ltOp
            A.GtOp -> TL.gtOp
            A.LeOp -> TL.leOp
            A.GeOp -> TL.geOp
            A.EqOp -> TL.eqOp
            A.NeqOp -> TL.neqOp

        (binExp, temp3) = 
          let
            c = trop oper
          in
           c e1 e2 temp''
           
        (strcmpExp, temp3s) =
          let
            op = trop oper
          in
           TL.strcmpExp e1 e2 op temp''
      in
         if check_result then
           case lty of
             T.STRING -> (ExpTy{expr=strcmpExp, ty=T.INT}, lv'', frgs'', temp3s)
             _ -> (ExpTy{expr=binExp, ty=T.INT}, lv'', frgs'', temp3)
         else 
           must_not_reach
                      
    trexp level frgs temp (A.VarExp var) = trvar level frgs temp var

    trexp level frgs temp A.RecordExp{A.fields=fields, A.typ=typ, A.pos=pos} = 
      case S.lookup tenv typ of
        Nothing -> error $ show pos ++ "record type not found: " ++ typ
        Just ty' -> case actual_ty ty' pos of
          T.RECORD ftys_ty u -> 
            let 
              (level', frgs', temp', ftys_exp) = 
                foldr
                (\(sym,e',pos') (l, f, t, xs) -> case trexp l f t e' of
                    (expty, l', f', t') -> (l', f', t', (sym, expty, pos'):xs)
                ) 
                (level, frgs, temp, [])
                fields
                
              (cs, {- level'' -} _, frgs'', temp'') =
                foldr
                (\(sym,_) (cs', lv, fs, tmp) ->
                  case lookup sym [(s,e')|(s,e',_)<-fields] of
                    Just e'' -> case trexp lv fs tmp e'' of
                      (ExpTy{expr=expr'}, l', f', t') -> (expr':cs', l', f', t')
                    _ -> must_not_reach
                )
                ([], level', frgs', temp')
                ftys_ty
                
              (e, temp3) = TL.recordExp cs temp''
            in
             if checkrecord ftys_ty ftys_exp pos
             then
               {- TODO: check level''? -}
               (ExpTy {expr=e, ty=T.RECORD ftys_ty u}, level', frgs'', temp3)
             else
               must_not_reach
          _ -> must_not_reach
      where
        checkrecord ftys_ty ftys_exp pos0 = 
          let
            checker (sym, ExpTy{ty=t2}, pos') = 
              case lookup sym ftys_ty of
                Just t1 -> check_type t1 t2 pos'
                Nothing -> error $ show pos0 ++ "field not found: " ++ sym
          in
            (length ftys_ty == length ftys_exp) && (and $ fmap checker ftys_exp)
        
    trexp level frgs temp (A.SeqExp exps) = 
      let 
        (lv', frgs', temp', es) = 
          foldr
          (\exp' (l, f, t, xs) -> case trexp l f t exp' of
              (e', l', f', t') -> (l', f', t', e':xs)
          )
          (level, frgs, temp, [])
          exps
        ty' = if null exps
              then T.UNIT
              else case last es of ExpTy{ty=typ} -> typ
        (e, temp'') = TL.seqExp [e' | ExpTy{expr=e'} <- es] temp'
      in
       (ExpTy{expr=e, ty=ty'}, lv', frgs', temp'')
               
    trexp level frgs temp A.AssignExp{A.vvar=var, A.exp=exp0, A.pos=pos} = 
      let 
        (ExpTy {expr=lhs, ty=vty }, lv', frgs', temp') = trvar level frgs temp var
        (ExpTy {expr=rhs, ty=ety }, lv'', frgs'', temp'') = trexp lv' frgs' temp' exp0
        (e, temp3) = TL.assignExp lhs rhs temp''
      in
       if check_type vty ety pos
       then (ExpTy {expr=e, ty=T.UNIT }, lv'', frgs'', temp3)
       else undefined
       
    trexp level frgs temp A.IfExp{ A.test=test, A.thene=thenexp, A.elsee=elseexp, 
                                   A.pos=pos} =
      let
        (ExpTy{expr=e1, ty=testty}, lv', frgs', temp') = trexp level frgs temp test
        (ExpTy{expr=e2, ty=thenty}, lv'', frgs'', temp'') = trexp lv' frgs' temp' thenexp
      in
       if check_type T.INT testty pos
       then
         case elseexp of
           Just elseexp' -> 
             let 
               (ExpTy{expr=e3, ty=elsety}, lv3, frgs3, temp3) = trexp lv'' frgs'' temp'' elseexp'
               (e, temp4) = TL.ifThenElse e1 e2 e3 temp3
             in
              if check_type thenty elsety pos then
                (ExpTy{expr=e, ty=thenty}, lv3, frgs3, temp4)
              else undefined
           Nothing -> if check_type T.UNIT thenty pos
                      then 
                        let
                          (e, temp3) = TL.ifThen e1 e2 temp''
                        in
                         (ExpTy{expr=e, ty=thenty}, lv'', frgs'', temp3)
                      else
                        undefined
       else
         undefined

    trexp level frgs temp A.WhileExp{A.test=test, A.body=body, A.pos=pos} =
      let
        (newdest, temp') = Temp.newLabel temp
        (ExpTy{expr=e1, ty=testty}, lv', frgs', temp'') = trexp level frgs temp' test
        (ExpTy{expr=e2, ty=bodyty}, lv'', frgs'', temp3) = 
          transExp venv tenv newdest lv' frgs' temp'' body
        (e, temp4) = TL.whileExp e1 e2 newdest temp3
      in
       if check_type T.INT testty pos && check_type T.UNIT bodyty pos
       then
         (ExpTy{expr=e, ty=T.UNIT}, lv'', frgs'', temp4)
       else
         undefined

    trexp level frgs temp (A.BreakExp _) = 
      (ExpTy {expr=TL.breakExp brkdest, ty=T.UNIT}, level, frgs, temp)
    
    trexp level frgs temp A.LetExp{A.decs=decs, A.body=body {-, A.pos=pos -}} =
      let
        transdecs (ve, te, lv, tmp, _, fs) dec = transDec ve te brkdest lv fs tmp dec
        (venv', tenv', {- lv' -} _, temp', es, frgs') = 
          foldl transdecs (venv, tenv, level, temp, [], frgs) decs
        (ExpTy {expr=ebody, ty=bodyty }, lv'', frgs'', temp'') = 
          transExp venv' tenv' brkdest level frgs' temp' body 
        (e, temp3) = TL.letExp es ebody temp''
      in
       (ExpTy{expr=e, ty=bodyty}, lv'', frgs'', temp3)

    trexp level frgs temp A.ArrayExp {A.typ=typ, A.size=size, A.init=init0,
                                      A.pos=pos} =
      case S.lookup tenv typ of
        Nothing -> error $ show pos ++ "type not found: " ++ typ
        Just t -> 
          let 
            ty1 = actual_ty t pos 
          in
           case ty1 of
             T.ARRAY ty' _ ->
               let 
                 (ExpTy{expr=siz, ty=sizety}, lv', frgs', temp') = trexp level frgs temp size
                 (ExpTy{expr=ini, ty=initty}, lv'', frgs'', temp'') = trexp lv' frgs' temp' init0
                 (e, temp3) = TL.arrayExp siz ini temp''
               in
                if check_type T.INT sizety pos && check_type ty' initty pos
                then
                  (ExpTy {expr=e, ty=ty1}, lv'', frgs'', temp3)
                else
                  undefined
             _ -> must_not_reach
                  
    trexp level frgs temp A.ForExp{A.svar=svar, A.lo=lo, A.hi=hi, A.body=body,
                                   A.pos=pos } =
      {- translate to let/while expresion -}
      let
        ivar = A.SimpleVar svar pos
        limitvar = A.SimpleVar "_limit" pos
        decs = [A.VarDec { A.name' = svar
                         , A.escape' = False
                         , A.typ' = Nothing
                         , A.init' = lo
                         , A.pos' = pos }
               ,A.VarDec { A.name' = "_limit"
                         , A.escape' = False
                         , A.typ' = Nothing
                         , A.init' = hi
                         , A.pos' = pos}
               ]
        loop = A.WhileExp { A.test = A.OpExp { A.oper = A.LeOp
                                             , A.lhs = A.VarExp ivar
                                             , A.rhs = A.VarExp limitvar
                                             , A.pos = pos }
                          , A.body = 
                               A.SeqExp [ body
                                        , A.AssignExp { 
                                          A.vvar = ivar,
                                          A.exp = A.OpExp {
                                            A.oper = A.PlusOp,
                                            A.lhs = A.VarExp ivar,
                                            A.rhs = A.IntExp 1 pos,
                                            A.pos = pos },
                                          A.pos = pos }
                                        ]
                          , A.pos = pos }
      in
       trexp level frgs temp A.LetExp{A.decs=decs, A.body=loop, A.pos=pos}
                                                  
    trexp level frgs temp A.CallExp{A.func=func, A.args=args, A.pos=pos} =
      case S.lookup venv func of
        Nothing -> error $ show pos ++ "function not defined: " ++ func
        Just (E.VarEntry _ _) -> 
          error $ show pos ++ "not a function: " ++ func
        Just E.FunEntry{E.label=label, E.formals=formals, E.result=result} ->
          let
            (lv', frgs', temp', argtys) =  
              foldr
              (\exp' (l, f, t, xs) -> case trexp l f t exp' of
                  (e', l', f', t') -> (l', f', t', e':xs))
              (level, frgs, temp, [])
              args
              
            checkformals fmls argtys' =
              let
                checker (t1, ExpTy {ty=t2}) = check_type t1 t2 pos
                ts = zip fmls argtys'
                szcheck = 
                  if (length fmls == length argtys') then
                    True
                  else
                    error $ show pos ++ "wrong number of arguments."
              in
               szcheck && (and $ fmap checker ts)
               
            es = fmap expr argtys
            sl = TL.fpExp lv'
            
            (e, temp'') = TL.callExp label (sl:es) temp'
          in
           if checkformals formals argtys
           then 
             (ExpTy{expr=e, ty=actual_ty result pos}, lv', frgs', temp'')
           else
             undefined

    trvar level frgs temp (A.SimpleVar sym pos) = 
      case S.lookup venv sym of
        Just E.VarEntry {E.access=acc, E.ty=ty1} 
          -> (ExpTy {expr=TL.simpleVar acc level, ty=ty1}, level, frgs, temp)
        Just _ -> error $ show pos ++ "not a variable: " ++ sym
        _ -> error $ show pos ++ "undefined variable: " ++ sym
    
    trvar level frgs temp (A.FieldVar var id' pos) = 
      let
        (ExpTy{expr=e1, ty=ty1}, lv', frgs', temp') = trvar level frgs temp var
      in
       case ty1 of
         T.RECORD fs _ ->
           case lookup id' [(s, (i, t))| (i, (s, t)) <- zip [0..] fs] of
             Nothing -> error $ show pos ++ "field not found: " ++ id'
             Just (i, ty') ->
               let
                 (e, temp'') = TL.fieldVar e1 i temp'
               in
                (ExpTy{expr=e, ty=actual_ty ty' pos}, lv', frgs', temp'')
         _ -> error $ show pos ++ "not a record: " ++ show ty1
         
    trvar level frgs temp (A.SubscriptVar var exp0 pos) = 
      let
        (ExpTy{expr=e1, ty=ty1}, lv', frgs', temp') = trvar level frgs temp var
      in
       case actual_ty ty1 pos of
         T.ARRAY ty' _ -> 
           let 
             (ExpTy{expr=e2, ty=ty''}, lv'', frgs'', temp'') = trexp lv' frgs' temp' exp0
             (e, temp3) = TL.subscriptVar e1 e2 temp''
           in
            case ty'' of
              T.INT -> (ExpTy {expr=e, ty=ty'}, lv'', frgs'', temp3)
              _ -> error $ show pos ++ "array subscript type:" ++ show ty''
         _ -> error $ show pos ++ "not an array"
  in
   trexp

transTy :: S.Table T.Ty -> A.Ty -> Bool -> T.Ty
transTy tenv =
  let
    -- dirty hack: generate a unique number from the position.
    pos2u (A.Pos l c) = fromIntegral $ l * 10000 + c
    
    transty (A.NameTy sym _) False =
      case S.lookup tenv sym of
        Just typ -> typ
        _ -> error "must not reach here, transy A.NameTy."
        
    transty (A.NameTy sym pos) True =
      let
        follow_ty seen sym0 =
          if List.all (/= sym0) seen then
            case S.lookup tenv sym0 of
              Just ty' -> 
                case ty' of
                  T.NAME s (Just (T.NAME s' _)) -> 
                    T.NAME s (Just $ follow_ty (s:seen) s')
                  _ -> ty'
              _ -> error "must not reach here, update A.NameTy. (2)"
          else
            {- must not reach here? -}
            error $ show pos ++ "cyclic dependency': " ++ sym0

      in
       case S.lookup tenv sym of
         Just ty' -> 
           case ty' of
             T.NAME s _ -> 
               case S.lookup tenv s of
                 Just (T.NAME s' (Just (T.NAME s'' _))) -> 
                   T.NAME s' (Just $ follow_ty [sym] s'')
                 Just ty1 -> ty1
                 Nothing -> must_not_reach
             _ -> ty'
         _ -> error "must not reach here, update A.NameTy."
    
    transty (A.RecordTy fs pos) _ =
      let
        f A.Field { A.field_name = name, A.field_typ = typ } = 
          case S.lookup tenv typ of
            Just ty' -> (name, ty') 
            Nothing -> error $ show pos ++ "type not defined (field): " ++ typ
      in
       if checkdup (fmap A.field_name fs) (fmap A.field_pos fs) then
         T.RECORD (fmap f fs) (pos2u pos)
       else
         undefined
       
    transty (A.ArrayTy sym pos) _ =
      case S.lookup tenv sym of
        Just ty' -> T.ARRAY ty' $ pos2u pos
        Nothing -> error $ show pos ++ "type not defined (array): " ++ sym
  in
   transty

transDec :: VEnv -> TEnv -> Temp.Label -> TL.Level -> [Frame.Frag] -> Temp.Temp
            -> A.Dec 
            -> (S.Table E.EnvEntry
               , S.Table T.Ty
               , TL.Level
               , Temp.Temp
               , [TL.Exp]
               , [Frame.Frag]
               )
transDec venv tenv brkdest =
  let
    trdec :: TL.Level -> [Frame.Frag] -> Temp.Temp
             -> A.Dec 
             -> (S.Table E.EnvEntry
                , S.Table T.Ty
                , TL.Level
                , Temp.Temp
                , [TL.Exp]
                , [Frame.Frag]
                )
    
    trdec level frgs temp A.VarDec{A.name'=name, A.typ'=typ, A.init'=init0, 
                                   A.escape'=esc, A.pos'=pos} = 
      let                                     
        (ExpTy{expr=rhs, ty=ty0}, lv', frgs', temp') =
          transExp venv tenv brkdest level frgs temp init0

        (access, lv'', temp'') = TL.allocLocal lv' esc temp'
        
        lhs = TL.simpleVar access lv''
        
        (e, temp3) = TL.assignExp lhs rhs temp''

        ret n ty1 = 
          (S.insert venv n E.VarEntry {E.access=access, E.ty=ty1}, 
           tenv, lv'', temp3, [e], frgs')
      in
       case typ of
         Nothing -> if ty0 == T.NIL
                    then
                      error $ 
                      show pos ++ "nil can be used only in the long form."
                    else
                      ret name ty0
         Just sym -> 
           case S.lookup tenv sym of
             Nothing -> error $ show pos ++ "type not found: " ++ sym
             Just ty' -> if check_type ty' ty0 pos
                         then
                           ret name ty0
                         else
                           undefined

    trdec level frgs temp (A.TypeDec tdecs) = 
      let
        {- inserting headers -}
        tenv' = 
          foldl 
          (\acc (name, _, _) -> S.insert acc name (T.NAME name Nothing))
          tenv
          tdecs
        
        {- transTy 1st pass-}
        tenv'' = 
          foldl
          (\acc (name, typ, _) -> 
            case S.lookup acc name of
              Just (T.NAME n _) -> 
                S.insert acc n $ T.NAME n (Just $ transTy acc typ False)
              _ -> error "must not reach here"
          )
          tenv'
          tdecs
        
        {- transTy 2nd pass: updating -}
        tenv''' = 
          foldl
          (\acc (name, typ, _) -> 
            case S.lookup acc name of
              Just (T.NAME n _) -> 
                S.insert acc n $ T.NAME n (Just $ transTy acc typ True)
              _ -> error "must not reach here."
          )
          tenv''
          tdecs

        names = fmap (\(n,_,_) -> n) tdecs
        poss = fmap (\(_,_,pos) -> pos) tdecs
        
        check_cyclic_dep [] = True
        check_cyclic_dep ((name, ty0, p):xs) = 
          let
            chkcyc seen typ pos' =
              case typ of
                Nothing -> error $ show pos' ++ "type not found: " ++ show ty0
                Just ty' ->
                  case ty' of
                    T.NAME sym ty'' ->
                      if (List.all (/= sym) seen) then
                        chkcyc (sym:seen) ty'' pos'
                      else
                        False
                    _ -> True
          in
           case S.lookup tenv''' name of
             Just (T.NAME _ typ) ->
               if chkcyc [name] typ p then
                 check_cyclic_dep xs
               else
                 error $ show p ++ "cyclic dependency: " ++ name
             _ -> error "must not reach here."
          
      in
        if check_cyclic_dep tdecs && checkdup names poss
        then
          (venv, tenv''', level, temp, [], frgs)
        else
          undefined
          
    trdec level frgs temp (A.FunctionDec fundecs) = 
      let
        {- 1st pass -}
        transfun (ve, tt) A.FuncDec{A.name=name, A.params=params, 
                                        A.result=result, {- A.func_body=body,-}
                                        A.func_pos=pos } = 
          let
            rty = 
              case result of
                Nothing -> T.UNIT
                Just typ -> 
                  case S.lookup tenv typ of
                    Nothing -> error $ show pos ++ "result type not found: " ++ show typ
                    Just t -> t
                    
            ftys = 
              fmap
              (\A.Field { A.field_typ = typ, A.field_pos = p } ->
                case S.lookup tenv typ of
                  Just t -> t
                  Nothing -> error $ show p ++ "type not found: " ++ typ)
              params
            
            (tlabel, t') = Temp.newLabel tt
            label = tlabel ++ "_" ++ name

            formals = fmap A.field_esc params

            -- (True:formals) corresponds to (sl:args)
            (lev, t'') = TL.newLevel level label (True:formals) t'
     
          in
           if checkdup (fmap A.field_name params) (fmap A.field_pos params) then
             (S.insert ve name E.FunEntry { E.level = lev
                                            , E.label = label
                                            , E.formals = ftys
                                            , E.result = rty
                                            },
              t'')
           else
             undefined

        (venv', temp') = foldl transfun (venv,temp) fundecs
        
        {- 2nd pass -}
        transbody
          (acc, {- level -} _, tmp, fs) -- level not used?
          A.FuncDec { A.name = name, A.params = params, 
                      {- A.result = result, -} A.func_body = body, 
                      A.func_pos = pos } = 
          let
            Just E.FunEntry { E.level = lev
                            , E.result = rty
                            , E.formals = formals } = 
              S.lookup venv' name
            
            transparam ve (A.Field{A.field_name=n}, t, a) =
              S.insert ve n $ E.VarEntry {E.access=a, E.ty=t}
            
            -- drop the access for the static_link.
            (_:as) = TL.acc_formals lev

            venv_loc = 
              foldl transparam venv' $ zip3 params formals as
            
            (ExpTy{expr=ebody, ty=bdty}, lv', fs', t') = 
              transExp venv_loc tenv brkdest lev fs tmp body
              
            -- TODO: unNx should not be public.
            (stm, t'') = TL.unNx t' ebody
              
            frag = Frame.Proc { Frame.get_body=stm
                              , Frame.get_frame=TL.frame lv'}
          in
           (check_type rty bdty pos && acc, lv', t'', frag:fs')
        
        (check_bodies, level', temp'', frgs') = 
          foldl transbody (True, level, temp', frgs) fundecs
      in
       if checkdup (fmap A.name fundecs) (fmap A.func_pos fundecs)
          && check_bodies 
       then
         (venv', tenv, level', temp'', [], frgs')
       else
         undefined
  in
   trdec

checkdup :: Show pos => [String] -> [pos] -> Bool
checkdup [] _ = True
checkdup (name:ns) (pos:ps) = 
  if List.all (/= name) ns then 
    checkdup ns ps
  else
    error $ show pos ++ "duplicated defintion: " ++ name
checkdup (_:_) [] = error "fatal: checkdup (_:_) []"
