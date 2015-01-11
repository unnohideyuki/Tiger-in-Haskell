module FrameBase where

import qualified Temp

data Access = InFrame Int
            | InReg Int
              deriving (Eq, Show)

class FrameBase frame where
  newFrame   :: Temp.Label -> [Bool] -> Temp.Temp -> (frame, Temp.Temp)
  formals    :: frame -> [Access]
  allocLocal :: frame -> Bool -> Temp.Temp -> (Access, frame, Temp.Temp)


