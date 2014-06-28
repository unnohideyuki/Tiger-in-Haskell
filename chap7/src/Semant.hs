module Semant where

import qualified Data.List as List

import qualified Absyn as A
import qualified Env as E
import qualified Symbol as S
import qualified Types as T
import qualified Translate as TL
import qualified Temp

type VEnv = S.Table E.EnvEntry
type TEnv = S.Table T.Ty
type Unique = T.Unique

data ExpTy = ExpTy {expr::TL.Exp, ty::T.Ty}
  
data Optype = Arith | Comp | Eq

actual_ty ty pos =
  case ty of
    T.NAME s t -> case t of
      Just ty' -> actual_ty ty' pos
      Nothing -> error $ show pos ++ "type not found (in actual_ty): " ++ s
    T.ARRAY ty' u -> T.ARRAY (actual_ty ty' pos) u
    _ -> ty

type_mismatch e a pos =
  error $ show pos ++ "type mismatch: expected " ++ show e ++ ", actual " ++ show a

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
       
must_not_reach =
  error "fatal: must not reach here"

transProg :: S.Table E.EnvEntry -> S.Table T.Ty
            -> A.Exp 
            -> ExpTy
            
transProg venv tenv prog = 
  let
    temp = Temp.create
    (mainlevel, temp') = 
      TL.newLevel TL.outermost (Temp.namedLabel "main") [] temp
  in
   case transExp venv tenv mainlevel temp' prog of
     (expty, _, _) -> expty

transExp :: S.Table E.EnvEntry -> S.Table T.Ty -> TL.Level -> Temp.Temp
            -> A.Exp 
            -> (ExpTy, TL.Level, Temp.Temp)
transExp venv tenv =
  let
    trexp :: TL.Level -> Temp.Temp
            -> A.Exp 
            -> (ExpTy, TL.Level, Temp.Temp)
            
    trexp level temp A.NilExp = (ExpTy (TL.nilExp) T.NIL, level, temp)
    
    trexp level temp (A.IntExp i _) = (ExpTy (TL.intExp i) T.INT, level, temp)
    
    trexp level temp (A.StringExp _ _) = (ExpTy undefined T.STRING, level, temp)
    
    trexp level temp A.OpExp{A.oper=oper, A.lhs=lhs, A.rhs=rhs, A.pos=pos} = 
      let
        (ExpTy {expr=e1, ty=lty }, lv', temp')   = trexp level temp lhs
        (ExpTy {expr=e2, ty=rty }, lv'', temp'') = trexp lv' temp' rhs
        
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
        
        check_int ty pos = 
          case ty of
            T.INT -> True
            _     -> error $ show pos ++ ": integer required."
            
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
            
        (binExp, temp3) = 
          let
            c = 
              case oper of
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
          in
           c e1 e2 temp''
      in
         if check_result then
           (ExpTy{expr=binExp, ty=T.INT}, lv'', temp3)
         else 
           must_not_reach
                      
    trexp level temp (A.VarExp var) = trvar level temp var

    trexp level temp A.RecordExp{A.fields=fields, A.typ=typ, A.pos=pos} = 
      case S.lookup tenv typ of
        Nothing -> error $ show pos ++ "record type not found: " ++ typ
        Just ty -> case actual_ty ty pos of
          T.RECORD ftys_ty u -> 
            let 
              (level', temp', ftys_exp) = 
                foldr
                (\(sym,e,pos) (l, t, xs) -> case trexp l t e of
                    (expty, l', t') -> (l', t', (sym, expty, pos):xs)
                ) 
                (level, temp, [])
                fields
                
              (cs, level'', temp'') =
                foldr
                (\(sym,_) (cs, level, temp) ->
                  case lookup sym [(s,e)|(s,e,_)<-fields] of
                    Just e -> case trexp level temp e of
                      (ExpTy{expr=expr}, l', t') -> (expr:cs, l', t')
                )
                ([], level', temp')
                ftys_ty
                
              (e, temp3) = TL.recordExp cs temp''
            in
             if checkrecord ftys_ty ftys_exp pos
             then
               (ExpTy {expr=e, ty=T.RECORD ftys_ty u}, level', temp3)
             else
               must_not_reach
      where
        checkrecord ftys_ty ftys_exp pos = 
          let
            checker (sym, ExpTy{ty=t2}, pos') = 
              case lookup sym ftys_ty of
                Just t1 -> check_type t1 t2 pos'
                Nothing -> error $ show pos ++ "field not found: " ++ sym
          in
            (length ftys_ty == length ftys_exp) && (and $ fmap checker ftys_exp)
        
    trexp level temp (A.SeqExp exps) = 
      let 
        (lv', temp', es) = 
          foldr
          (\exp (l, t, xs) -> case trexp l t exp of
              (e, l', t') -> (l', t', e:xs)
          )
          (level, temp, [])
          exps
        ty' = if null exps
              then T.UNIT
              else case last es of ExpTy{ty=ty} -> ty
      in
       (ExpTy{expr=TL.dummy, ty=ty'}, lv', temp')
               
    trexp level temp A.AssignExp{A.vvar=var, A.exp=exp, A.pos=pos} = 
      let 
        (ExpTy { ty=vty }, lv', temp') = trvar level temp var
        (ExpTy { ty=ety }, lv'', temp'') = trexp lv' temp' exp
      in
       if check_type vty ety pos
       then (ExpTy {expr=undefined, ty=T.UNIT }, lv'', temp'')
       else undefined
       
    trexp level temp A.IfExp{ A.test=test, A.thene=thenexp, A.elsee=elseexp, 
                    A.pos=pos} =
      let
        (ExpTy{expr=e1, ty=testty}, lv', temp') = trexp level temp test
        (ExpTy{expr=e2, ty=thenty}, lv'', temp'') = trexp lv' temp' thenexp
      in
       if check_type T.INT testty pos
       then
         case elseexp of
           Just elseexp' -> 
             let 
               (ExpTy{expr=e3, ty=elsety}, lv3, temp3) = trexp lv'' temp'' elseexp'
               (e, temp4) = TL.ifThenElse e1 e2 e3 temp3
             in
              if check_type thenty elsety pos then
                (ExpTy{expr=e, ty=thenty}, lv3, temp4)
              else undefined
           Nothing -> if check_type T.UNIT thenty pos
                      then 
                        let
                          (e, temp3) = TL.ifThen e1 e2 temp''
                        in
                         (ExpTy{expr=e, ty=thenty}, lv'', temp3)
                      else
                        undefined
       else
         undefined

    trexp level temp A.WhileExp{A.test=test, A.body=body, A.pos=pos} =
      let
        (ExpTy{ty=testty}, lv', temp') = trexp level temp test
        (ExpTy{ty=bodyty}, lv'', temp'') = trexp lv' temp' body
      in
       if check_type T.INT testty pos && check_type T.UNIT bodyty pos
       then
         (ExpTy{expr=undefined, ty=T.UNIT}, lv'', temp'')
       else
         undefined

    trexp level temp (A.BreakExp _) = 
      (ExpTy {expr=undefined, ty=T.UNIT}, level, temp)
    
    trexp level temp A.LetExp{A.decs=decs, A.body=body, A.pos=pos} =
      let
        transdecs (venv, tenv, lv, tmp) dec = transDec venv tenv lv tmp dec
        (venv', tenv', lv', temp') = 
          foldl transdecs (venv, tenv, level, temp) decs
        (ExpTy { ty=bodyty }, lv'', temp'') = 
          transExp venv' tenv' lv' temp' body
      in
       (ExpTy{expr=undefined, ty=bodyty}, lv'', temp'')

    trexp level temp A.ArrayExp {A.typ=typ, A.size=size, A.init=init,
                       A.pos=pos} =
      case S.lookup tenv typ of
        Nothing -> error $ show pos ++ "type not found: " ++ typ
        Just t -> 
          let 
            ty = actual_ty t pos 
          in
           case ty of
             T.ARRAY ty' u ->
               let 
                 (ExpTy{ty=sizety}, lv', temp') = trexp level temp size
                 (ExpTy{ty=initty}, lv'', temp'') = trexp lv' temp' init
               in
                if check_type T.INT sizety pos && check_type ty' initty pos
                then
                  (ExpTy {expr=undefined, ty=ty}, lv'', temp'')
                else
                  undefined
                  
    trexp level temp A.ForExp{A.svar=svar, A.lo=lo, A.hi=hi, A.body=body,
                     A.pos=pos } =
      {- translate to let/while expresion -}
      let
        ivar = A.SimpleVar svar pos
        limitvar = A.SimpleVar "limit" pos
        decs = [A.VarDec { A.name' = svar
                         , A.escape' = False
                         , A.typ' = Nothing
                         , A.init' = lo
                         , A.pos' = pos }
               ,A.VarDec { A.name' = "limit"
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
       trexp level temp A.LetExp{A.decs=decs, A.body=loop, A.pos=pos }
                                                  
    trexp level temp A.CallExp{A.func=func, A.args=args, A.pos=pos } =
      case S.lookup venv func of
        Nothing -> error $ show pos ++ "function not defined: " ++ func
        Just (E.VarEntry _ _) -> 
          error $ show pos ++ "not a function: " ++ func
        Just E.FunEntry{E.formals=formals, E.result=result } ->
          let
            (lv', temp', argtys) =  
              foldr
              (\exp (l, t, xs) -> case trexp l t exp of
                  (e, l', t') -> (l', t', e:xs))
              (level, temp, [])
              args
            checkformals formals argtys =
              let
                checker (t1, ExpTy { ty=t2 }) = check_type t1 t2 pos
                ts = zip formals argtys
                szcheck = 
                  if (length formals == length argtys) then
                    True
                  else
                    error $ show pos ++ "wrong number of arguments."
              in
               szcheck && (and $ fmap checker ts)
          in
           if checkformals formals argtys
           then 
             (ExpTy {expr=undefined, ty=actual_ty result pos}, level, temp)
           else
             undefined

    trvar level temp (A.SimpleVar sym pos) = 
      case S.lookup venv sym of
        Just E.VarEntry {E.access=acc, E.ty=ty} 
          -> (ExpTy {expr=TL.simpleVar acc level, ty=ty}, level, temp)
        Just _ -> error $ show pos ++ "not a variable: " ++ sym
        _ -> error $ show pos ++ "undefined variable: " ++ sym
    
    trvar level temp (A.FieldVar var id pos) = 
      let
        (ExpTy{expr=e1, ty=ty}, lv', temp') = trvar level temp var
      in
       case ty of
         T.RECORD fs _ ->
           case lookup id [(s, (i, ty))| (i, (s, ty)) <- zip [0..] fs] of
             Nothing -> error $ show pos ++ "field not found: " ++ id
             Just (i, ty') ->
               let
                 (e, temp'') = TL.fieldVar e1 i temp'
               in
                (ExpTy{expr=e, ty=actual_ty ty' pos}, lv', temp'')
         _ -> error $ show pos ++ "not a record: " ++ show ty
         
    trvar level temp (A.SubscriptVar var exp pos) = 
      let
        (ExpTy{expr=e1, ty=ty}, lv', temp') = trvar level temp var
      in
       case actual_ty ty pos of
         T.ARRAY ty' _ -> 
           let 
             (ExpTy{expr=e2, ty=ty''}, lv'', temp'') = trexp lv' temp' exp
             (e, temp3) = TL.subscriptVar e1 e2 temp''
           in
            case ty'' of
              T.INT -> (ExpTy {expr=e, ty=ty'}, lv'', temp3)
              _ -> error $ show pos ++ "array subscript type:" ++ show ty''
         _ -> error $ show pos ++ "not an array"
  in
   trexp

transTy :: S.Table T.Ty -> A.Ty -> Bool -> T.Ty
transTy tenv =
  let
    -- dirty hask: generate a unique number from the position.
    pos2u (A.Pos l c) = fromIntegral $ l * 10000 + c
    
    transty (A.NameTy sym pos) False =
      case S.lookup tenv sym of
        Just ty -> ty
        _ -> error "must not reach here, transy A.NameTy."
        
    transty (A.NameTy sym pos) True =
      let
        follow_ty seen sym =
          if List.all (/= sym) seen then
            case S.lookup tenv sym of
              Just ty -> 
                case ty of
                  T.NAME s (Just (T.NAME s' _)) -> 
                    T.NAME s (Just $ follow_ty (s:seen) s')
                  _ -> ty
              _ -> error "must not reach here, update A.NameTy. (2)"
          else
            {- must not reach here? -}
            error $ show pos ++ "cyclic dependency': " ++ sym

      in
       case S.lookup tenv sym of
         Just ty -> 
           case ty of
             T.NAME s _ -> 
               case S.lookup tenv s of
                 Just (T.NAME s' (Just (T.NAME s'' _))) -> 
                   T.NAME s' (Just $ follow_ty [sym] s'')
                 Just ty -> ty
             _ -> ty
         _ -> error "must not reach here, update A.NameTy."
    
    transty (A.RecordTy fs pos) _ =
      let
        f A.Field { A.field_name = name, A.field_typ = typ } = 
          case S.lookup tenv typ of
            Just ty -> (name, ty) 
            Nothing -> error $ show pos ++ "type not defined (field): " ++ typ
      in
       if checkdup (fmap A.field_name fs) (fmap A.field_pos fs) then
         T.RECORD (fmap f fs) (pos2u pos)
       else
         undefined
       
    transty (A.ArrayTy sym pos) _ =
      case S.lookup tenv sym of
        Just ty -> T.ARRAY ty $ pos2u pos
        Nothing -> error $ show pos ++ "type not defined (array): " ++ sym
  in
   transty

transDec :: S.Table E.EnvEntry -> S.Table T.Ty -> TL.Level -> Temp.Temp
            -> A.Dec 
            -> (S.Table E.EnvEntry, S.Table T.Ty, TL.Level, Temp.Temp)
transDec venv tenv =
  let
    trdec :: TL.Level -> Temp.Temp
             -> A.Dec 
             -> (S.Table E.EnvEntry, S.Table T.Ty, TL.Level, Temp.Temp)
    
    trdec level temp A.VarDec{A.name'=name, A.typ'=typ, A.init'=init, 
                              A.escape'=esc, A.pos'=pos} = 
      let                                     
        (ExpTy{ty=ty}, lv', temp') =
          transExp venv tenv level temp init

        (access, lv'', temp'') = TL.allocLocal lv' esc temp'

        ret name ty = 
          (S.insert venv name E.VarEntry {E.access=access, E.ty=ty}, 
           tenv, lv'', temp'')
      in
       case typ of
         Nothing -> if ty == T.NIL
                    then
                      error $ 
                      show pos ++ "nil can be used only in the long form."
                    else
                      ret name ty
         Just sym -> 
           case S.lookup tenv sym of
             Nothing -> error $ show pos ++ "type not found: " ++ sym
             Just ty' -> if check_type ty' ty pos
                         then
                           ret name ty
                         else
                           undefined

    trdec level temp (A.TypeDec tdecs) = 
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
          (\acc (name, ty, _) -> 
            case S.lookup acc name of
              Just (T.NAME n _) -> 
                S.insert acc n $ T.NAME n (Just $ transTy acc ty False))
          tenv'
          tdecs
        
        {- transTy 2nd pass: updating -}
        tenv''' = 
          foldl
          (\acc (name, ty, _) -> 
            case S.lookup acc name of
              Just (T.NAME n _) -> 
                S.insert acc n $ T.NAME n (Just $ transTy acc ty True))
          tenv''
          tdecs

        names = fmap (\(n,_,_) -> n) tdecs
        poss = fmap (\(_,_,pos) -> pos) tdecs
        
        check_cyclic_dep [] = True
        check_cyclic_dep ((name, ty, pos):xs) = 
          let
            chkcyc seen ty pos =
              case ty of
                Nothing -> error $ show pos ++ "type not found: " ++ show ty
                Just ty' ->
                  case ty' of
                    T.NAME sym ty'' ->
                      if (List.all (/= sym) seen) then
                        chkcyc (sym:seen) ty'' pos
                      else
                        False
                    _ -> True
          in
           case S.lookup tenv''' name of
             Just (T.NAME _ ty) ->
               if chkcyc [name] ty pos then
                 check_cyclic_dep xs
               else
                 error $ show pos ++ "cyclic dependency: " ++ name
          
      in
        if check_cyclic_dep tdecs && checkdup names poss
        then
          (venv, tenv''', level, temp)
        else
          undefined
          
    trdec level temp (A.FunctionDec fundecs) = 
      let
        {- 1st pass -}
        transfun (venv, temp) A.FuncDec{A.name=name, A.params=params, 
                                        A.result=result, A.func_body=body, 
                                        A.func_pos=pos } = 
          let
            rty = 
              case result of
                Nothing -> T.UNIT
                Just typ -> 
                  case S.lookup tenv typ of
                    Nothing -> error $ show pos ++ "result type not found: " ++ show typ
                    Just ty -> ty
                    
            ftys = 
              fmap
              (\A.Field { A.field_typ = typ, A.field_pos = pos } ->
                case S.lookup tenv typ of
                  Just t -> t
                  Nothing -> error $ show pos ++ "type not found: " ++ typ)
              params
            
            (label, temp') = Temp.newLabel temp

            formals = fmap A.field_esc params

            (lev, temp'') = TL.newLevel level label formals temp'
     
          in
           if checkdup (fmap A.field_name params) (fmap A.field_pos params) then
             (S.insert venv name E.FunEntry { E.level = lev
                                            , E.label = label
                                            , E.formals = ftys
                                            , E.result = rty
                                            },
              temp'')
           else
             undefined

        (venv', temp') = foldl transfun (venv,temp) fundecs
        
        {- 2nd pass -}
        transbody
          (acc, level, temp)
          A.FuncDec { A.name = name, A.params = params, 
                      A.result = result, A.func_body = body, 
                      A.func_pos = pos } = 
          let
            Just E.FunEntry { E.result = rty, E.formals = formals } = 
              S.lookup venv' name
            
            transparam acc (A.Field { A.field_name = name }, ty) =
              S.insert acc name $ E.VarEntry { E.access = undefined
                                             , E.ty=ty }

            venv_loc = foldl transparam venv' $ zip params formals
            
            (ExpTy{ty=bdty}, lv', temp') = 
              transExp venv_loc tenv level temp body
          in
           (check_type rty bdty pos && acc, lv', temp')
        
        (check_bodies, level', temp'') = 
          foldl transbody (True, level, temp') fundecs
      in
       if checkdup (fmap A.name fundecs) (fmap A.func_pos fundecs)
          && check_bodies 
       then
         (venv', tenv, level', temp'')
       else
         undefined
       
  in
   trdec

checkdup [] _ = True
checkdup (name:ns) (pos:ps) = 
  if List.all (/= name) ns then 
    checkdup ns ps
  else
    error $ show pos ++ "duplicated defintion: " ++ name
