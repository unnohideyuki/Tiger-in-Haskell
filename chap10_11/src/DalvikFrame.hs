module DalvikFrame (module DalvikFrame, module FrameBase) where

import FrameBase
import qualified Temp
import Data.Foldable (foldr')
import qualified Tree as T

data Frame = Frame { name :: Temp.Label
                   , formals' :: [Access] 
                   , locals :: [Access]
                   , fp :: Int
                   , sp :: Int
                   , arcd :: Int
                   }
             deriving (Eq, Show)

instance FrameBase Frame where
  newFrame = newFrame'
  formals = formals'
  allocLocal = allocLocal'
  
data Frag = Proc {get_body::T.Stm, get_frame::Frame}
          | Str Temp.Label String
            deriving Show

newFrame' :: Temp.Label -> [Bool] -> Temp.Temp -> (Frame, Temp.Temp)
newFrame' label fs temp = 
  let
    calc_formals (escapes, n) (acc, t) =
      if escapes then
        (InFrame (-3 - n) : acc, t)
      else
        let
          (m, t') = Temp.newTemp t
        in
         (InReg m : acc, t')
         
    (fmls, temp') = foldr' calc_formals ([], temp) $ zip fs [0..]
    
    (t1, temp'') = Temp.newTemp temp'
    (t2, temp3) = Temp.newTemp temp''
    (t3, temp4) = Temp.newTemp temp3
    
    frame = Frame { name = label
                  , formals' = fmls
                  , locals = []
                  , fp = t1
                  , sp = t2
                  , arcd = t3
                  }
  in
   (frame, temp4)
   
allocLocal' :: Frame -> Bool -> Temp.Temp -> (Access, Frame, Temp.Temp)
allocLocal' f@Frame{locals=ls} escapes temp =
  let
    nlocals = length ls
    
    (l, temp') = if escapes then
                   (InFrame $ nlocals + 1, temp)
                 else
                   let
                     (m, t') = Temp.newTemp temp
                   in
                    (InReg m, t')
                   
    locals' = l : ls
  in
   (l, f{locals=locals'}, temp')

exp :: Access -> T.Exp -> T.Exp
exp (InFrame k) fpexp =
  T.MEM $ T.BINOP T.PLUS (T.CONST k) fpexp
exp (InReg t) _ =  
  T.TEMP t

static_link :: T.Exp -> T.Exp
static_link fpexp = T.MEM $ T.BINOP T.PLUS (T.CONST (-2)) fpexp
