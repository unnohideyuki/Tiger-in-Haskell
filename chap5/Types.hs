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
          deriving (Eq, Show)
                   
next :: Unique -> Unique
next i = i + 1
