module Temp where

import qualified Symbol

type Temp = (Int, Int)
type Label = Symbol.Symbol

newTemp :: Temp
newTemp = undefined

makeString :: Temp -> String
makeString = undefined

newLabel :: Temp -> Label
newLabel = undefined

namedLabel :: String -> Label
namedLabel = Symbol.fromString



