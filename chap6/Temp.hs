module Temp where

import qualified Symbol

data Temp = Temp { temps :: Int, labs :: Int }
type Label = Symbol.Symbol

create :: Temp
create = Temp { temps = 0, labs = 0 }

newTemp :: Temp -> (Int, Temp)
newTemp t@Temp{temps=temps}  = (temps, t{ temps = temps + 1})

makeString :: Int -> String
makeString n = "t" ++ show n

newLabel :: Temp -> (Label, Temp)
newLabel t@Temp{labs=labs} = 
  let
    label = Symbol.fromString $ "L" ++ show labs
    labs' = labs + 1
  in
   (label, t{labs=labs'})

namedLabel :: String -> Label
namedLabel = Symbol.fromString
