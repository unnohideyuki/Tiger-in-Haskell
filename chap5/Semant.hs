module Semant where

import qualified Data.List as List

import qualified Absyn as A
import qualified Env as E
import qualified Symbol as S
import qualified Types as T

type VEnv = S.Table E.EnvEntry
type TEnv = S.Table T.Ty
type Unique = T.Unique

data ExpTy = ExpTy { ty :: T.Ty } -- Translate.Exp has not been prepared yet.
             deriving (Show)

  
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

transExp venv tenv =
  let
    trexp A.NilExp = ExpTy T.NIL
    
    trexp (A.IntExp _ _) = ExpTy T.INT
    
    trexp (A.StringExp _ _) = ExpTy T.STRING
    
    trexp A.OpExp{A.oper=oper, A.lhs=lhs, A.rhs=rhs, A.pos=pos} = 
      let
        ExpTy { ty=lty } = trexp lhs
        ExpTy { ty=rty } = trexp rhs
        
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
            
      in
         if check_result then
           ExpTy { ty=T.INT }
         else 
           must_not_reach
                      
    trexp (A.VarExp var) = trvar var

    trexp A.RecordExp { A.fields = fields, A.typ = typ, A.pos = pos} = 
      case S.lookup tenv typ of
        Nothing -> error $ show pos ++ "record type not found: " ++ typ
        Just ty -> case actual_ty ty pos of
          T.RECORD ftys_ty u -> 
            let 
              ftys_exp = fmap (\(_,e,pos) -> (trexp e, pos)) fields
            in
             if checkrecord ftys_ty ftys_exp pos
             then
               ExpTy { ty = T.RECORD ftys_ty u }
             else
               must_not_reach
      where
        checkrecord ftys_ty ftys_exp pos = 
          let
            checker ((_,t1), (ExpTy{ty=t2},pos')) = check_type t1 t2 pos'
            fs = zip ftys_ty ftys_exp
          in
            (length ftys_ty == length ftys_exp) && (and $ fmap checker fs)
        
    trexp (A.SeqExp exps) = 
      let 
        es = fmap trexp exps
        ty = if null exps
             then T.UNIT 
             else case last es of ExpTy { ty=ty' } -> ty'
      in
       ExpTy { ty = ty }
               
    trexp A.AssignExp { A.vvar = var, A.exp = exp, A.pos = pos } = 
      let 
        ExpTy { ty=vty } = trvar var
        ExpTy { ty=ety } = trexp exp
      in
       if check_type vty ety pos
       then ExpTy { ty=T.UNIT }
       else undefined
       
    trexp A.IfExp { A.test = test, A.thene = thenexp, A.elsee = elseexp, 
                    A.pos = pos} =
      let
        ExpTy { ty=testty } = trexp test
        ExpTy { ty=thenty } = trexp thenexp
      in
       if check_type T.INT testty pos
       then
         case elseexp of
           Just elseexp' -> let ExpTy { ty=elsety } = trexp elseexp'
                            in
                             if check_type thenty elsety pos
                             then ExpTy { ty=thenty } else undefined
           Nothing -> if check_type T.UNIT thenty pos
                      then 
                        ExpTy { ty=thenty }
                      else
                        undefined
       else
         undefined

    trexp A.WhileExp { A.test = test, A.body = body, A.pos = pos } =
      let
        ExpTy { ty=testty } = trexp test
        ExpTy { ty=bodyty } = trexp body
      in
       if check_type T.INT testty pos && check_type T.UNIT bodyty pos
       then
         ExpTy { ty=T.UNIT }
       else
         undefined

    trexp (A.BreakExp _) = ExpTy { ty=T.UNIT }
    
    trexp A.LetExp { A.decs = decs, A.body = body, A.pos = pos } =
      let
        transdecs (venv, tenv) dec = transDec venv tenv dec
        (venv', tenv') = foldl transdecs (venv, tenv) decs
        ExpTy { ty=bodyty } = transExp venv' tenv' body
      in
       ExpTy { ty=bodyty }

    trexp A.ArrayExp { A.typ = typ, A.size = size, A.init = init,
                       A.pos = pos } =
      case S.lookup tenv typ of
        Nothing -> error $ show pos ++ "type not found: " ++ typ
        Just t -> 
          let 
            ty = actual_ty t pos 
          in
           case ty of
             T.ARRAY ty' u ->
               let 
                 ExpTy { ty=sizety } = trexp size
                 ExpTy { ty=initty } = trexp init
               in
                if check_type T.INT sizety pos && check_type ty' initty pos
                then
                  ExpTy { ty = ty }
                else
                  undefined
                  
    trexp A.ForExp { A.svar = svar, A.lo = lo, A.hi = hi, A.body = body,
                     A.pos = pos } =
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
       trexp A.LetExp {A.decs = decs, A.body = loop, A.pos = pos }
                                                  
    trexp A.CallExp { A.func = func, A.args = args, A.pos = pos } =
      case S.lookup venv func of
        Nothing -> error $ show pos ++ "function not defined: " ++ func
        Just (E.VarEntry _) -> error $ show pos ++ "not a function: " ++ func
        Just E.FunEntry { E.formals = formals, E.result = result } ->
          let
            argtys = fmap trexp args
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
             ExpTy { ty = actual_ty result pos }
           else
             undefined

    trvar (A.SimpleVar sym pos) = 
      case S.lookup venv sym of
        Just E.VarEntry { E.ty=ty } -> ExpTy { ty=ty }
        Just _ -> error $ show pos ++ "not a variable: " ++ sym
        _ -> error $ show pos ++ "undefined variable: " ++ sym
    
    trvar (A.FieldVar var id pos) = 
      let
        ExpTy { ty=ty } = trvar var
      in
       case ty of
         T.RECORD fs _ ->
           case lookup id fs of
             Nothing -> error $ show pos ++ "field not found: " ++ id
             Just ty' -> ExpTy { ty = actual_ty ty' pos }
         _ -> error $ show pos ++ "not a record: " ++ show ty
         
    trvar (A.SubscriptVar var exp pos) = 
      let
        ExpTy { ty=ty } = trvar var
      in
       case actual_ty ty pos of
         T.ARRAY ty' _ -> 
           let ExpTy { ty=ty'' } = trexp exp
           in
            case ty'' of
              T.INT -> ExpTy { ty=ty' }
              _ -> error $ show pos ++ "array subscript type:" ++ show ty''
         _ -> error $ show pos ++ "not an array"
  in
   trexp

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

transDec venv tenv =
  let
    trdec A.VarDec { A.name' = name, A.typ' = typ, A.init' = init, 
                     A.pos' = pos} = 
      let                                     
        ExpTy { ty=ty } = transExp venv tenv init
        ret name ty = 
          (S.insert venv name E.VarEntry { E.ty = ty }, tenv)
      in
       case typ of
         Nothing -> if ty == T.NIL
                    then
                      error $ show pos ++ "nil can be used only in the long form."
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

    trdec (A.TypeDec tdecs) = 
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
          (venv, tenv''')
        else
          undefined
          
    trdec (A.FunctionDec fundecs) = 
      let
        {- 1st pass -}
        transfun venv A.FuncDec { A.name = name, A.params = params, 
                                  A.result = result, A.func_body = body, 
                                  A.func_pos = pos } = 
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
          in
           if checkdup (fmap A.field_name params) (fmap A.field_pos params) then
             S.insert venv name E.FunEntry { E.formals = ftys
                                           , E.result = rty
                                           }
           else
             undefined

        venv' = foldl transfun venv fundecs
        
        {- 2nd pass -}
        transbody acc A.FuncDec { A.name = name, A.params = params, 
                                  A.result = result, A.func_body = body, 
                                  A.func_pos = pos } = 
          let
            Just E.FunEntry { E.result = rty, E.formals = formals } = 
              S.lookup venv' name
            
            transparam acc (A.Field { A.field_name = name }, ty) =
              S.insert acc name $ E.VarEntry { E.ty=ty }

            venv_loc = foldl transparam venv' $ zip params formals
            
            ExpTy { ty=bdty } = transExp venv_loc tenv body
          in
           check_type rty bdty pos && acc
        
        check_bodies = foldl transbody True fundecs
      in
       if checkdup (fmap A.name fundecs) (fmap A.func_pos fundecs)
          && check_bodies 
       then
         (venv', tenv)
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
