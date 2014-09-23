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

get_sp :: State CgenState Int
get_sp = state $ \st -> (F.sp $ get_frame st, st)

get_fp :: State CgenState Int
get_fp = state $ \st -> (F.fp $ get_frame st, st)

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

munchExp (T.CALL (T.NAME f) es) =
  let
    nargs = length es
    
    v0 = -1
    v1 = -2

    setargs [] _ _ = return ()
    setargs (e:es') n sp =
      if n < 3
      then do
        t <- munchExp e
        emit $ A.regMoveInstr (-3-n) t
        setargs es' (n+1) sp
      else do
        t <- munchExp e
        i <- munchExp $ T.BINOP T.MINUS (T.TEMP sp) (T.CONST $ 3 + n)
        munchStm [T.MOVE (T.MEM $ T.TEMP i) (T.TEMP t)]
        setargs es' (n+1) sp
  in
   do
     arcd <- get_arcd
     fp <- get_fp
     sp <- get_sp
     nsp <- munchExp $ T.BINOP T.PLUS (T.TEMP sp) (T.CONST $ nargs+3)
     emit $ A.regMoveInstr v0 arcd
     emit $ A.regMoveInstr v1 fp
     i <- munchExp $ T.BINOP T.MINUS (T.TEMP nsp) (T.CONST 2)
     munchStm [T.MOVE (T.MEM $ T.TEMP i) (T.TEMP fp)]
     setargs es 0 nsp
     t <- newTemp
     emit $ A.callInstr f t nargs
     return t

munchExp (T.CALL f _) = fail $ "not a function name" ++ show f
munchExp (T.NAME _) = fail "NAME alone is not expected to be munchExped."
munchExp (T.ESEQ _ _) = fail "ESEQ must not appear in this phase."                       

munchStm :: [T.Stm] -> State CgenState ()

munchStm [] = return ()

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
    
munchStm ((T.MOVE (T.TEMP 0) v):ss) = -- Move to (TEMP 0) is special
  do
    s0 <- munchExp v
    emit $ A.returnInstr s0
    munchStm ss
    
munchStm ((T.MOVE (T.TEMP d0) v):ss) = -- move-object
  do
    s0 <- munchExp v
    emit $ A.regMoveInstr d0 s0
    munchStm ss

munchStm ((T.MOVE e _):_) = fail $ "not a left-value: " ++ (show e)

munchStm ((T.EXP e):ss) =
  do
    _ <- munchExp e
    munchStm ss

munchStm ((T.JUMP (T.NAME label) _):ss) =
  do
    emit $ A.jumpInstr label
    munchStm ss

munchStm ((T.JUMP l _):_) = fail $ "not a jump destination: " ++ show l

munchStm ((T.CJUMP rel e0 e1 lt lf):(T.LABEL l):ss) =
  let
    cond = case rel of
               T.EQ -> "eq"
               T.NE -> "ne"
               T.LT -> "lt"
               T.LE -> "le"
               T.GT -> "gt"
               T.GE -> "ge"
               _ -> fail $ "unsupported relop" ++ (show rel)
  in
   do
     if lf /= l
       then fail $ "unmatched label: " ++ lf ++ ", " ++ l
       else return ()
     s0 <- munchExp e0
     s1 <- munchExp e1
     t0 <- newTemp
     t1 <- newTemp
     emit $ A.cjumpInstr cond [t0, t1] [s0, s1] lt
     munchStm ss
     
munchStm ((T.CJUMP _ _ _ _ _):_) = fail "CJUMP must be followed by LABEL."

munchStm ((T.SEQ _ _):_) = fail "T.SEQ must not appear in this phase."

munchStm ((T.LABEL l):ss) =
  do
    emit $ A.labelDef l
    munchStm ss