module Translate ( Level, Access
                 , outermost, newLevel, allocLocal
                 ) where

import qualified Temp
import qualified Frame
import qualified DalvikFrame as Frame
import qualified Tree as T

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

data Exp = Ex T.Exp
         | Nx T.Stm
         | Cx (Temp.Label -> Temp.Label -> T.Stm)

unEx :: Temp.Temp -> Exp -> (T.Exp, Temp.Temp)
unEx temp = 
  let
    unex (Ex e) = (e, temp)

    unex (Cx genstm) =
      let (r, temp') = Temp.newTemp temp
          (t, temp'') = Temp.newLabel temp'
          (f, temp3) = Temp.newLabel temp''

          e = T.ESEQ [T.MOVE (T.TEMP r) (T.CONST 1),
                      genstm t f,
                      T.LABEL f,
                      T.MOVE (T.TEMP r) (T.CONST 0),
                      T.LABEL t]
              $ T.TEMP r
      in
       (e, temp3)
       
    unex (Nx s) = (T.ESEQ [s] $ T.CONST 0, temp)
  in
   unex



    