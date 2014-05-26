module Absyn where

data Pos = Pos { line :: Int, column :: Int }
           deriving(Show)

type Symbol = String -- TODO: It may not be the best

data Var = SimpleVar Symbol Pos
         | FieldVar Var Symbol Pos
         | SubscriptVar Var Exp Pos
           deriving(Show)

data Exp = VarExp Var
         | NilExp
         | IntExp Integer Pos
         | StringExp String Pos
         | CallExp { func :: Symbol, args :: [Exp], pos :: Pos }
         | OpExp { oper :: Oper, lhs :: Exp, rhs :: Exp, pos :: Pos}
         | RecordExp { fields :: [(Symbol, Exp, Pos)], typ :: Symbol, pos :: Pos }
         | SeqExp [Exp]
         | AssignExp { vvar :: Var, exp :: Exp, pos :: Pos }
         | IfExp  { test :: Exp, thene :: Exp, elsee :: Maybe Exp, pos :: Pos }
         | WhileExp { test :: Exp, body :: Exp, pos :: Pos }
         | ForExp { svar :: Symbol, escape :: Bool, 
                    lo :: Exp, hi :: Exp, body :: Exp, pos :: Pos }
         | BreakExp Pos
         | LetExp { decs :: [Dec], body :: Exp, pos :: Pos }
         | ArrayExp { typ :: Symbol, size :: Exp, init :: Exp, pos :: Pos }
           deriving(Show)

data Dec = FunctionDec [FuncDec]
         | VarDec { name' :: Symbol, escape' :: Bool, 
                    typ' :: Maybe Symbol, init' :: Exp, pos' :: Pos }
         | TypeDec [(Symbol, Ty, Pos)]
           deriving(Show)
           
data Ty = NameTy Symbol Pos
        | RecordTy [Field]
        | ArrayTy Symbol Pos
          deriving(Show)
          
data Oper = PlusOp | MinusOp | TimesOp | DivideOp 
          | EqOp | NeqOp | LtOp | LeOp | GtOp | GeOp
          deriving(Show)

data Field = Field { field_name :: Symbol, field_esc :: Bool, 
                     field_typ :: Symbol, field_pos :: Pos }
             deriving(Show)

data FuncDec = FuncDec { name :: Symbol, params :: [Field],
                         result :: Maybe Symbol, func_body :: Exp, func_pos :: Pos }
               deriving(Show)

           
           
