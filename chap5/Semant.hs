module Semant where

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
  {- TODO: to detect cyclic dependency -}
  case ty of
    T.NAME s t -> case t of
      Just ty' -> actual_ty ty' pos
      Nothing -> error $ show pos ++ "undefined type: " ++ s
    T.ARRAY ty' u -> T.ARRAY (actual_ty ty' pos) u
    _ -> ty

typeMismatch e a pos =
  error $ show pos ++ "type mismatch: expected " ++ show e ++ ", actual " ++ show a

checkType t1 t2 pos =
  let
    t1' = actual_ty t1 pos
    t2' = actual_ty t2 pos
  in
   if t1' /= t2'
   then
     case (t1', t2') of
       (T.RECORD _ _, T.NIL) -> True
       (T.NIL, T.RECORD _ _) -> True
       _ -> typeMismatch t1' t2' pos
   else True
       
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
        
        checkInt ty pos = 
          case ty of
            T.INT -> True
            _     -> error $ show pos ++ ": integer required."
            
        checkArith = checkInt lty pos && checkInt rty pos
        
        checkEq = 
          case lty of
            T.INT -> checkType lty rty pos
            T.STRING -> checkType lty rty pos
            T.ARRAY _ _ -> checkType lty rty pos
            T.RECORD _ _ -> checkType lty rty pos
            T.NIL -> checkType lty rty pos
            _ -> error $ show pos ++ "type error for equality operator: " ++ show lty
            
        checkComp =
          case lty of
            T.INT -> checkType lty rty pos
            T.STRING -> checkType lty rty pos
            _ -> error $ show pos ++ "type error for comparison: " ++ show lty
            
      in
       case classify oper of
         Arith -> if checkArith 
                  then ExpTy { ty=T.INT }
                  else undefined
         Comp -> if checkComp
                 then ExpTy { ty=T.INT }
                 else undefined
         Eq -> if checkEq
               then ExpTy { ty=T.INT }
               else undefined
                      
    trexp (A.VarExp var) = trvar var

    trexp A.RecordExp { A.fields = fields, A.typ = typ, A.pos = pos} = 
      case S.lookup tenv typ of
        Nothing -> error $ show pos ++ "record type not found: " ++ typ
        Just ty -> case actual_ty ty pos of
          T.RECORD ftys_from_ty u -> 
            let 
              ftys_from_exp = fmap (\(_,e,pos) -> (trexp e, pos)) fields
            in
             if checkrecord ftys_from_ty ftys_from_exp pos
             then
               ExpTy { ty = T.RECORD ftys_from_ty u }
             else
               undefined
      where
        checkrecord ftys_ty ftys_exp pos = 
          let
            checker ((_,t1), (ExpTy{ty=t2},pos')) = checkType t1 t2 pos'
            fs = zip ftys_ty ftys_exp
            szcheck = (length ftys_ty == length ftys_exp)
          in
           szcheck && (and $ fmap checker fs)
        
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
       if checkType vty ety pos
       then ExpTy { ty=T.UNIT }
       else undefined
       
    trexp A.IfExp { A.test = test, A.thene = thenexp, A.elsee = elseexp, 
                    A.pos = pos} =
      let
        ExpTy { ty=testty } = trexp test
        ExpTy { ty=thenty } = trexp thenexp
      in
       if checkType T.INT testty pos
       then
         case elseexp of
           Just elseexp' -> let ExpTy { ty=elsety } = trexp elseexp'
                            in
                             if checkType thenty elsety pos
                             then ExpTy { ty=thenty } else undefined
           Nothing -> if checkType T.UNIT thenty pos
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
       if checkType T.INT testty pos && checkType T.UNIT bodyty pos
       then
         ExpTy { ty=T.UNIT }
       else
         undefined

    trexp (A.BreakExp _) = ExpTy { ty=T.UNIT }
    
    trexp A.LetExp { A.decs = decs, A.body = body, A.pos = pos } =
      let
        (venv', tenv') = transdecs venv tenv decs
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
                if checkType T.INT sizety pos && checkType ty' initty pos
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
                checker (t1, ExpTy { ty=t2 }) = checkType t1 t2 pos
                ts = zip formals argtys
                szcheck = (length formals == length argtys)
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
    
    transty (A.NameTy sym pos) =
      case S.lookup tenv sym of
        Just ty -> ty
        _ -> error "must not reach here"
        
    transty (A.RecordTy fs pos) =
      let
        f A.Field { A.field_name = name } = 
          case S.lookup tenv name of
            Just ty -> (name, ty)
            Nothing -> error $ show pos ++ "undefined type: " ++ name
      in
       T.RECORD (fmap f fs) (pos2u pos)
       
    transty (A.ArrayTy sym pos) =
      case S.lookup tenv sym of
        Just ty -> T.ARRAY ty $ pos2u pos
        Nothing -> error $ show pos ++ "undefined type: " ++ sym
  in
   transty

transdecs = undefined
