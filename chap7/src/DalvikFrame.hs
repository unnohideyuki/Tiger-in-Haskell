module DalvikFrame where

import qualified Frame as F
import qualified Temp
import Data.Foldable (foldr')
import qualified Tree as T

data Frame = Frame { name :: Temp.Label
                   , formals :: [F.Access] 
                   , locals :: [F.Access]
                   , fp :: Int
                   }
             deriving (Eq, Show)

instance F.FrameBase Frame where
  newFrame = newFrame'
  formals = formals
  allocLocal = allocLocal'
  
data Frag = Proc {body::T.Stm, frame::Frame}
          | Str Temp.Label String

newFrame' :: Temp.Label -> [Bool] -> Temp.Temp -> (Frame, Temp.Temp)
newFrame' label fs temp = 
  let
    calc_formals (escapes, n) (acc, t) =
      if escapes then
        (F.InFrame (-n) : acc, temp)
      else
        let
          (m, temp') = Temp.newTemp temp
        in
         (F.InReg m : acc, temp')
         
    (formals, temp') = foldr' calc_formals ([], temp) $ zip fs [0..]
    
    (fp, temp'') = Temp.newTemp temp'
    
    frame = Frame { name = label
                  , formals = formals
                  , locals = []
                  , fp = fp
                  }
  in
   (frame, temp'')
   
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

exp :: F.Access -> T.Exp -> T.Exp
exp (F.InFrame k) fpexp =
  T.MEM $ T.BINOP T.PLUS (T.CONST k) fpexp
exp (F.InReg t) _ =  
  T.TEMP t

static_link :: T.Exp -> T.Exp
static_link fpexp = T.MEM $ T.BINOP T.PLUS (T.CONST (-3)) fpexp
