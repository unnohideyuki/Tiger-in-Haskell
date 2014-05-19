-- Tigerbook PROGRAM 1.5
-- Representation of straight-line programs.

module Prog1_5 where

type Id = String

data Binop = Plus | Minus | Times | Div

data Stm = CompoundStm Stm Stm
         | AssignStm Id Exp
         | PrintStm [Exp]

data Exp = IdExp Id
         | NumExp Int
         | OpExp Binop Exp Exp
         | EseqExp Stm Exp

-- Example prog:
--   a := 5 + 3; b := (print (a, a-1), 10 * a); print (b)

stm1 = AssignStm "a" (OpExp Plus (NumExp 5) (NumExp 3))  -- a := 5 + 3
stm2 = PrintStm [IdExp "a", OpExp Minus (IdExp "a") (NumExp 1)]  -- print (a, a-1)
exp3 = OpExp Times (NumExp 10) (IdExp "a") -- 10 * a
stm4 = AssignStm "b" $ EseqExp stm2 exp3
stm5 = PrintStm [IdExp "b"] -- print (b)

prog = CompoundStm stm1 $ CompoundStm stm4 stm5
