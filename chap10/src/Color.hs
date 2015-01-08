module Color where

import Control.Monad.State

import qualified Data.Map as Map
import Data.List

import qualified Graph as G

type Interference = Map.Map Int [Int]
type AdjSet = Map.Map (Int, Int) Bool
type GMap = Map.Map G.Node [Int]

data BuildState = BuildState { adjSet :: AdjSet
                             , adjList :: Interference
                             }


build :: [G.Node] -> GMap -> GMap -> Map.Map G.Node Bool -> GMap -> Interference
build nodes defMap useMap isMoveMap liveOutMap = fst $ runState (build' nodes) newBuildState
  where
    wrapmap :: GMap -> G.Node -> [Int]
    wrapmap m node =
      case Map.lookup node m of
        Just xs -> xs
        _ -> []

    ds = wrapmap defMap
    us = wrapmap useMap
    liveOut = wrapmap liveOutMap

    isMove node =
      case Map.lookup node isMoveMap of
        Just True -> True
        _ -> False

    newBuildState = BuildState Map.empty Map.empty

    retResult = state $ \st -> (adjList st, st)
    
    build' [] = retResult
    build' (n:ns) = do
      let live = liveOut n
          live' = if isMove n then live \\ us n else live
          live'' = union live' $ ds n
      addEdges (ds n) live''
      build' ns

    addEdges :: [Int] -> [Int] -> State BuildState ()
    addEdges [] _ = return ()
    addEdges (d:xs) live = do
      addEdges2 d live
      addEdges xs live

    addEdges2 :: Int -> [Int] -> State BuildState ()
    addEdges2 _ [] = return ()
    addEdges2 d (l:ls) = do
      addEdge d l
      addEdges2 d ls

    addEdge :: Int -> Int -> State BuildState ()
    addEdge u v = state $ \st ->
      let
        aset0 = adjSet st

        isNewEdge =
          case Map.lookup (u, v) aset0 of
            Just True -> False
            _ -> (u /= v)
        
        aset' = Map.insert (u, v) True aset0
        aset'' = Map.insert (v, u) True aset'
        aset = if isNewEdge then aset'' else aset0

        alist0 = adjList st
        alist' =
          case Map.lookup u alist0 of
            Just xs -> Map.insert u (v:xs) alist0
            _ -> Map.insert u [v] alist0
            
        alist'' =
          case Map.lookup v alist' of
            Just xs -> Map.insert v (u:xs) alist'
            _ -> Map.insert v [u] alist'
            
        alist = if isNewEdge then alist'' else alist0
      in
       ((), st{adjSet=aset, adjList=alist})

data AssignColorStatus =  AssignColorStatus
                          { selectStack :: [Int]
                          , colorMap :: Map.Map Int Int
                          }

assignColor :: Interference -> [Int] -> Map.Map Int Int 
assignColor adjListMap selectStack0 = cmap
  where
    k = 14 -- number of colors (registers)

    adjlist n = case Map.lookup n adjListMap of
      Just xs -> xs
      _ -> []

    newStatus stack = AssignColorStatus stack Map.empty

    assignLoop :: State AssignColorStatus (Map.Map Int Int)
    assignLoop = do
      n <- pop
      adjNodes <- adjacency n
      okColors <- calcOkColors adjNodes [0..(k-1)]
      colorNode n okColors
      sz <- stackSize
      if sz > 0 then assignLoop else retColMap

    pop :: State AssignColorStatus Int
    pop = state $ \st ->
      let
        stack = selectStack st
        stack' = tail stack
        n = head stack
      in
       (n, st{selectStack=stack'})

    adjacency :: Int -> State AssignColorStatus [Int]
    adjacency n = state $ \st ->
      let
        stack = selectStack st
      in
       (adjlist n \\ stack, st)

    calcOkColors :: [Int] -> [Int] -> State AssignColorStatus [Int]
    calcOkColors [] cols = return cols
    calcOkColors (w:ws) cols = do
      cols' <- checkcol w cols
      calcOkColors ws cols'

    checkcol :: Int -> [Int] -> State AssignColorStatus [Int]
    checkcol w cols = state $ \st ->
      let
        colmap = colorMap st
        cols' = case Map.lookup w colmap of
          Just c -> delete c cols
          _ -> cols
      in
       (cols', st)

    colorNode :: Int -> [Int] -> State AssignColorStatus ()
    colorNode _ [] = error "spilt!"
    colorNode n cols = state $ \st ->
      let
        c = head cols
        colmap = colorMap st
        colmap' = Map.insert n c colmap
      in
       ((), st{colorMap=colmap'})

    stackSize :: State AssignColorStatus Int
    stackSize = state $ \st ->
      let
        stack = selectStack st
      in
       (length stack, st)

    retColMap :: State AssignColorStatus (Map.Map Int Int)
    retColMap = state $ \st -> (colorMap st, st)

    (cmap, _) = runState assignLoop $ newStatus selectStack0
    
