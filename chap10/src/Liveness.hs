module Liveness where

import Control.Monad.State

import Data.List
import qualified Data.Map as Map

import qualified Graph as G

data LiveState = LiveState { get_liveIn :: Map.Map G.Node [Int]
                           , get_liveOut :: Map.Map G.Node [Int]
                           , get_liveIn' :: Map.Map G.Node [Int]
                           , get_liveOut' :: Map.Map G.Node [Int]
                           , get_def :: Map.Map G.Node [Int]
                           , get_use :: Map.Map G.Node [Int]
                           , get_graph :: G.Graph
                           }
                 deriving Show

newLiveState :: (Map.Map G.Node [Int]) -> (Map.Map G.Node [Int]) ->
                G.Graph -> LiveState
newLiveState ds us g = LiveState { get_liveIn = Map.empty
                                 , get_liveOut = Map.empty
                                 , get_liveIn' = Map.empty
                                 , get_liveOut' = Map.empty
                                 , get_def = ds
                                 , get_use = us
                                 , get_graph = g         
                                 }

liveIn :: G.Node -> State LiveState [Int]
liveIn node = state $ \st ->
  let
    lives =
      case Map.lookup node (get_liveIn st) of
        Just xs -> xs
        _ -> []
  in
   (lives, st)

liveIn' :: G.Node -> State LiveState [Int]
liveIn' node = state $ \st ->
  let
    lives =
      case Map.lookup node (get_liveIn' st) of
        Just xs -> xs
        _ -> []
  in
   (lives, st)

liveOut :: G.Node -> State LiveState [Int]
liveOut node = state $ \st ->
  let
    lives =
      case Map.lookup node (get_liveOut st) of
        Just xs -> xs
        _ -> []
  in
   (lives, st)

liveOut' :: G.Node -> State LiveState [Int]
liveOut' node = state $ \st ->
  let
    lives =
      case Map.lookup node (get_liveOut' st) of
        Just xs -> xs
        _ -> []
  in
   (lives, st)

setIn :: G.Node -> [Int] -> State LiveState ()
setIn node is = state $ \st ->
  let
    m = get_liveIn st
    m' = Map.insert node is m
  in
   ((), st{get_liveIn=m'})

setIn' :: G.Node -> [Int] -> State LiveState ()
setIn' node is = state $ \st ->
  let
    m = get_liveIn' st
    m' = Map.insert node is m
  in
   ((), st{get_liveIn'=m'})

setOut :: G.Node -> [Int] -> State LiveState ()
setOut node is = state $ \st ->
  let
    m = get_liveOut st
    m' = Map.insert node is m
  in
   ((), st{get_liveOut=m'})

setOut' :: G.Node -> [Int] -> State LiveState ()
setOut' node is = state $ \st ->
  let
    m = get_liveOut' st
    m' = Map.insert node is m
  in
   ((), st{get_liveOut'=m'})

def :: G.Node -> State LiveState [Int]
def node = state $ \st ->
  let
    ds =
      case Map.lookup node (get_def st) of
        Just xs -> xs
        _ -> []
  in
   (ds, st)

use :: G.Node -> State LiveState [Int]
use node = state $ \st ->
  let
    us =
      case Map.lookup node (get_use st) of
        Just xs -> xs
        _ -> []
  in
   (us, st)

succ_nodes :: G.Node -> State LiveState [G.Node]
succ_nodes node = state $ \st ->
  let
    ss = G.succ_nodes (get_graph st) node
  in
   (ss, st)

coll_ins :: [G.Node] -> [Int] -> State LiveState [Int]
coll_ins [] is = state $ \st -> (is, st)
coll_ins (n:ns) is =
  do
    is' <- liveIn n
    coll_ins ns (union is is')

nodes :: State LiveState [G.Node]
nodes = state $ \st ->
  let
    g = get_graph st
    n = G.get_nnodes g
    xs = reverse [0..(n-1)]
  in
   (fmap G.Node xs, st)

updateInOut :: [G.Node] -> State LiveState ()
updateInOut [] = return ()
updateInOut (n:ns) = do
  in' <- liveIn n
  setIn' n in'
  out' <- liveOut n
  setOut' n out'
  us <- use n
  os <- liveOut n
  ds <- def n
  setIn n (union us $ os \\ ds)
  s <- succ_nodes n
  succ_ins <- coll_ins s []
  setOut n succ_ins
  updateInOut ns

exitCondition :: [G.Node] -> Bool -> State LiveState Bool
exitCondition _ False = return False
exitCondition [] True = return True
exitCondition (n:ns) True = do
  is' <- liveIn' n
  is <- liveIn n
  os' <- liveOut n
  os <- liveOut n
  exitCondition ns (is' == is && os' == os)

returnOuts :: State LiveState (Map.Map G.Node [Int])
returnOuts = state $ \st ->
  let
    outs = get_liveOut st
  in
   (outs, st)

calcLiveness' :: State LiveState (Map.Map G.Node [Int])
calcLiveness' = do
  ns <- nodes
  updateInOut ns
  c <- exitCondition ns True
  case c of
    True -> returnOuts
    False -> calcLiveness'

calcLiveness ds us g =
  let
    st = newLiveState ds us g
    (outs, _) = runState calcLiveness' st
  in
   outs


  
  

  
