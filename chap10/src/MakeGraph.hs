module MakeGraph where

import Control.Monad.State
import qualified Data.Map as Map

import Flow
import qualified Assem as A
import qualified Graph as G
import qualified Temp

data NodeRef = NextInstr
             | Labeled Temp.Label
             | Known G.Node

instrs2graph :: [A.Instr] -> (Flowgraph, [G.Node])
instrs2graph = undefined

data I2gState = I2gState { labs :: Map.Map Temp.Label G.Node
                         , pends :: [(NodeRef, NodeRef)]
                         , flowg :: Flowgraph
                         , nodes :: [G.Node]
                         }
                
newState :: I2gState
newState = I2gState { labs = Map.empty
                    , pends = []
                    , flowg = FGRAPH { get_control = G.newGraph
                                     , get_def = Map.empty
                                     , get_use = Map.empty
                                     , get_ismove = Map.empty
                                     }
                    , nodes = []
                    }
           
newNode' :: State I2gState G.Node
newNode' = state $ \st ->
  let
    fg = flowg st
    g = get_control fg
    (node, g') = G.newNode g
    fg' = fg{get_control=g'}
    ns = nodes st
  in
   (node, st{flowg=fg', nodes=ns ++ [node]})
           
checkNextEdge :: G.Node -> State I2gState ()   
checkNextEdge node = state $ \st ->
  let
    fg = flowg st
    g = get_control fg
  in
   case pends st of
     ((Known nf, NextInstr):ps) -> 
       let
         g' = G.mk_edge g nf node
         fg' = fg{get_control=g'}
       in
        ((), st{flowg=fg'})
     _ -> ((), st)
   
newNode :: State I2gState G.Node
newNode = do
  node <- newNode'
  checkNextEdge node
  return node

insertLabel :: Temp.Label -> G.Node -> State I2gState ()
insertLabel lab node = state $ \st ->
  let
    m = labs st
    m' = Map.insert lab node m
  in
   ((), st{labs=m'})

set_def :: G.Node -> [Temp.Temp] -> State I2gState ()
set_def node ts = state $ \st ->
  let 
    fg = flowg st
    def = get_def fg
    def' = Map.insert node ts def
    fg' = fg{get_def=def'}
  in
   ((), st{flowg=fg'})
    
set_use :: G.Node -> [Temp.Temp] -> State I2gState ()
set_use node ts = state $ \st ->
  let 
    fg = flowg st
    use = get_use fg
    use' = Map.insert node ts use
    fg' = fg{get_use=use'}
  in
   ((), st{flowg=fg'})
    
set_ismove :: G.Node -> Bool -> State I2gState ()
set_ismove node b = state $ \st ->
  let 
    fg = flowg st
    ismove = get_ismove fg
    ismove' = Map.insert node b ismove
    fg' = fg{get_ismove=ismove'}
  in
   ((), st{flowg=fg'})
    
trInsts :: [A.Instr] -> State I2gState ()

trInsts (A.LABEL{A.lab_lab=lab}:insts) =
  do
    node <- newNode
    insertLabel lab node
    set_def node []
    set_use node []
    set_ismove node False
    -- TODO: solve labeled edge
