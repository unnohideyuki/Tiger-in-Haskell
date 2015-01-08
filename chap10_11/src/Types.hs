module Types where

import qualified Symbol

type Unique = Integer

data Ty = INT
        | STRING
        | RECORD  [(Symbol.Symbol, Ty)] Unique
        | ARRAY Ty Unique
        | NIL
        | UNIT
        | NAME Symbol.Symbol (Maybe Ty)
          deriving (Show)
                   
instance Eq Ty where
  INT          == INT           = True
  STRING       == STRING        = True
  (RECORD _ u) == (RECORD _ u') = u == u'
  (ARRAY _ u)  == (ARRAY _ u')  = u == u'
  NIL          == NIL           = True
  UNIT         == UNIT          = True
  (NAME s t)   == (NAME s' t')  = (s == s') && (t == t')
  _              == _               = False
