module Translate ( Level, Access
                 , outermost, newLevel, allocLocal
                 ) where

import qualified Temp
import qualified Frame
import qualified DalvikFrame as Frame

data Level = Level { parent :: Level
                   , name :: Temp.Label
                   , formals :: [Bool]
                   , frame :: Frame.Frame
                   }
           | Outermost
             deriving (Eq, Show)

data Access = Access { level :: Level, access :: Frame.Access }
              deriving (Eq, Show)

outermost = Outermost

newLevel :: Level -> Temp.Label -> [Bool] -> Temp.Temp -> (Level, Temp.Temp)
newLevel parent name formals temp = 
  let
    (label, temp') = Temp.newLabel temp
    (frame, temp'') = Frame.newFrame label formals temp'
  in
   (Level { parent = parent ,name = label, formals=formals, frame=frame },
    temp'')

allocLocal :: Level -> Bool -> Temp.Temp -> (Access, Level, Temp.Temp)
allocLocal level@Level{frame=frame} escapes temp =
  let
    (access, frame', temp') = Frame.allocLocal frame escapes temp
  in
   (Access{level=level, access=access}, level{frame=frame'}, temp')

