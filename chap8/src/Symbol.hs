module Symbol where

import qualified Data.Map as Map

type Symbol = String
type Table = Map.Map Symbol

fromString:: String -> String
fromString = id

empty :: Map.Map k a
empty = Map.empty

insert :: Ord k => Map.Map k a -> k -> a -> Map.Map k a
insert table s v = Map.insert s v table

lookup :: Ord k => Map.Map k a -> k -> Maybe a
lookup table s = Map.lookup s table
