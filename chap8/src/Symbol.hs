module Symbol where

import qualified Data.Map as Map

type Symbol = String
type Table = Map.Map Symbol

fromString:: String -> String
fromString = id

empty :: Table a
empty = Map.empty

insert :: Table a -> Symbol -> a -> Table a
insert table s v = Map.insert s v table

lookup :: Table a -> Symbol -> Maybe a
lookup table s = Map.lookup s table
