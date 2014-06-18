module Tree where

import qualified Temp

data Exp = CONST Int
         | NAME Temp.Label
         | TEMP Int
         | BINOP Binop Exp Exp
         | MEM Exp
         | CALL Exp [Exp]
         | ESEQ [Stm] Exp

data Stm = MOVE Exp Exp
         | EXP Exp
         | JUMP Exp [Temp.Label]
         | CJUMP Relop Exp Exp Temp.Label Temp.Label
         | SEQ Stm Stm
         | LABEL Temp.Label
           
data Binop = PLUS | MINUS | MUL | DIV
           | AND | OR | LSHIFT | RSHIFT | ARSHIFT | XOR
                                                    
data Relop = EQ | NE | LT | LE | GT | GE
           | ULT | ULE | UGT | UGE
