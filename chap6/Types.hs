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
  x@INT          == y@INT           = True
  x@STRING       == y@STRING        = True
  x@(RECORD _ u) == y@(RECORD _ u') = u == u'
  x@(ARRAY _ u)  == y@(ARRAY _ u')  = u == u'
  x@NIL          == y@NIL           = True
  x@UNIT         == y@UNIT          = True
  x@(NAME s t)   == y@(NAME s' t')  = (s == s') && (t == t')
  _              == _               = False
