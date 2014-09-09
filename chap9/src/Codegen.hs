module Codegen where

import Control.Monad.State

import qualified Assem as A
import qualified Tree as T
import qualified DalvikFrame as F
import qualified Temp

data InstsTemp = InstsTemp { get_insts :: [A.Instr]
                           , get_temp :: Temp.Temp 
                           , get_frame :: F.Frame }

munchExp :: T.Exp -> State InstsTemp Int

munchExp (T.CONST n) = 
  state $ \st -> 
  let
    insts = get_insts st
    temp = get_temp st
    (t, temp') = Temp.newTemp temp
    inst = A.constInstr n [t]
  in
   (t, st{get_insts=insts ++ [inst], get_temp=temp'})
   
munchExp (T.STR s) =
  state $ \st ->
  let
    insts = get_insts st
    temp = get_temp st
    (t, temp') = Temp.newTemp temp
    inst = A.strInstr s [t]
  in
   (t, st{get_insts=insts ++ [inst], get_temp=temp'})
    
munchExp (T.TEMP t) = return t

munchExp (T.BINOP op e1 e2) = 
  let
    binInstr = case op of
      T.PLUS -> A.addInstr
      T.MINUS -> A.subInstr
      T.MUL -> A.mulInstr
      T.DIV -> A.divInstr
      _ -> error "not support binInstr"
    
    munchBinop src =
      state $ \st ->
      let
        insts = get_insts st
        temp = get_temp st
        (d0, temp1) = Temp.newTemp temp
        (d1, temp2) = Temp.newTemp temp1
        (d2, temp3) = Temp.newTemp temp2
        inst = binInstr [d0, d1, d2] src
      in
       (d0, st{get_insts=(insts ++ [inst]), get_temp=temp3})
  in
   do
     t1 <- munchExp e1
     t2 <- munchExp e2
     munchBinop [t1, t2]
    
munchExp (T.MEM e) =
  let
    get_arcd = state $ \st -> (F.arcd $ get_frame st, st)
  in
   do
     t <- get_arcd
     munchExp (T.ARR (T.TEMP t) e)

munchExp (T.ARR e1 e2) =
  let
    munchArr src =
      state $ \st ->
      let
        insts = get_insts st
        temp = get_temp st
        (d0, temp') = Temp.newTemp temp
        (d1, temp'') = Temp.newTemp temp'
        inst = A.memInstr [d0, d1] src
      in
       (d0, st{get_insts=insts ++ [inst], get_temp=temp''})
  in
   do
     s1 <- munchExp e1
     s2 <- munchExp e2
     munchArr [s1, s2]
  
munchExp (T.RCD e i) = munchExp (T.ARR e (T.CONST i))

