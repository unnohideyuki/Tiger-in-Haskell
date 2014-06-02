module Frame where

import qualified Temp

data Access = InFrame Int
            | InReg Temp.Temp

class Frame frame where
  newFrame   :: Temp.Label -> [Bool] -> frame
  formals    :: frame -> [Access]
  allocLocal :: frame -> Bool -> Access

