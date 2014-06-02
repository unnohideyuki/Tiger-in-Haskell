module Symbol where

import qualified Data.Map as Map

type Symbol = String
type Table = Map.Map Symbol

empty = Map.empty
insert table s v = Map.insert s v table
lookup table s = Map.lookup s table




