module DalvikFrame where

import qualified Frame as F
import qualified Temp

data Frame = Frame { name :: Temp.Label
                   , formals :: [F.Access] 
                   , locals :: [F.Access]
                   }

instance F.FrameBase Frame where
  newFrame = newFrame'
  formals = formals
  allocLocal = allocLocal'
  
newFrame' :: Temp.Label -> [Bool] -> Temp.Temp -> (Frame, Temp.Temp)
newFrame' label fs temp = 
  let
    calc_formals (acc, t) (escapes, n) =
      if escapes then
        (F.InFrame (-n) : acc, temp)
      else
        let
          (m, temp') = Temp.newTemp temp
        in
         (F.InReg m : acc, temp')
         
    (formals, temp') = foldl calc_formals ([], temp) $ zip fs [0..]
    
    frame = Frame { name = label
                  , formals = formals
                  , locals = []
                  }
                    
  in
   (frame, temp')
   
   
   
allocLocal' :: Frame -> Bool -> Temp.Temp -> (F.Access, Frame, Temp.Temp)
allocLocal' f@Frame{locals=locals} escapes temp =
  let
    nlocals = length locals
    
    (l, temp') = if escapes then
                   (F.InFrame $ nlocals + 1, temp)
                 else
                   let
                     (m, temp') = Temp.newTemp temp
                   in
                    (F.InReg m, temp')
                   
    locals' = l : locals
  in
   (l, f{locals=locals'}, temp')
