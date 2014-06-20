module Tree where

import qualified Temp

data Exp = CONST Int
         | NAME Temp.Label
         | TEMP Int
         | BINOP Binop Exp Exp
         | MEM Exp
         | ARR Exp Exp
         | RCD Exp Int
         | CALL Exp [Exp]
         | ESEQ Stm Exp
           deriving(Eq, Show)

data Stm = MOVE Exp Exp
         | EXP Exp
         | JUMP Exp [Temp.Label]
         | CJUMP Relop Exp Exp Temp.Label Temp.Label
         | SEQ Stm Stm
         | LABEL Temp.Label
           deriving(Eq, Show)
           
data Binop = PLUS | MINUS | MUL | DIV
           | AND | OR | LSHIFT | RSHIFT | ARSHIFT | XOR
           deriving(Eq, Show)
                                                    
data Relop = EQ | NE | LT | LE | GT | GE
           | ULT | ULE | UGT | UGE
           deriving(Eq, Show)