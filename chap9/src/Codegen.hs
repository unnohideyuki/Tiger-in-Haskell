module Codegen where

import Control.Monad.State

import qualified Assem as A
import qualified Tree as T
import qualified DalvikFrame as F
import qualified Temp

data CgenState = CgenState { get_insts :: [A.Instr]
                           , get_temp :: Temp.Temp 
                           , get_frame :: F.Frame }

-- Methods of the State Monad

newTemp :: State CgenState Int
newTemp = state $ \st ->
  let
    temp = get_temp st
    (t, temp') = Temp.newTemp temp
  in
   (t, st{get_temp=temp'})
   
emit :: A.Instr -> State CgenState ()
emit inst = state $ \st ->
  let
    insts = get_insts st
  in
   ((), st{get_insts=insts++[inst]})
   
get_arcd :: State CgenState Int
get_arcd = state $ \st -> (F.arcd $ get_frame st, st)

-- munchExp

munchExp :: T.Exp -> State CgenState Int

munchExp (T.CONST n) = 
  do
    t <- newTemp
    emit $ A.constInstr n [t]
    return t
   
munchExp (T.STR s) =
  do
    t <- newTemp
    emit $ A.strInstr s [t]
    return t
    
munchExp (T.TEMP t) = return t

munchExp (T.BINOP op e1 e2) = 
  let
    binInstr = case op of
      T.PLUS -> A.addInstr
      T.MINUS -> A.subInstr
      T.MUL -> A.mulInstr
      T.DIV -> A.divInstr
      _ -> error "not support binInstr"
  in
   do
     t1 <- munchExp e1
     t2 <- munchExp e2
     d0 <- newTemp
     d1 <- newTemp
     d2 <- newTemp
     emit $ binInstr [d0, d1, d2] [t1, t2]
     return d0
    
munchExp (T.MEM e) =
  do
    t <- get_arcd
    munchExp (T.ARR (T.TEMP t) e)

munchExp (T.ARR e1 e2) =
  do
    s0 <- munchExp e1
    s1 <- munchExp e2
    d0 <- newTemp
    d1 <- newTemp
    emit $ A.memInstr [d0, d1] [s0, s1]
    return d0
  
munchExp (T.RCD e i) = munchExp (T.ARR e (T.CONST i))


munchStm :: [T.Stm] -> State CgenState ()

munchStm [] = return ()

munchStm ((T.MOVE (T.TEMP 0) v):ss) =
  do
    s0 <- munchExp v
    emit $ A.returnInstr s0
    munchStm ss

munchStm ((T.MOVE (T.MEM i) v):ss) =
  do
    d0 <- get_arcd
    d1 <- newTemp
    s0 <- munchExp i
    s1 <- munchExp v
    emit $ A.moveInstr [d0, d1] [s0, s1]
    munchStm ss

munchStm ((T.MOVE (T.ARR a i) v):ss) =
  do
    d0 <- munchExp a
    d1 <- newTemp
    s0 <- munchExp i
    s1 <- munchExp v
    emit $ A.moveInstr [d0, d1] [s0, s1]
    munchStm ss
    
munchStm ((T.MOVE (T.RCD r i) v):ss) =
  do
    d0 <- munchExp r
    d1 <- newTemp
    s0 <- munchExp (T.CONST i)
    s1 <- munchExp v
    emit $ A.moveInstr [d0, d1] [s0, s1]
    munchStm ss
    
munchStm ((T.MOVE (T.TEMP _) _):ss) = undefined

munchStm ((T.EXP e):ss) =
  do
    _ <- munchExp e
    munchStm ss



