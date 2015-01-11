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
instrs2graph insts =
  let
    (_, s) = runState (trInsts insts) newState
  in
   (flowg s, nodes s)

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
           
   
newNode :: State I2gState G.Node
newNode = do
  node <- newNode'
  checkNextEdge node
  return node
  where
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
            ((), st{flowg=fg', pends=ps})
         _ -> ((), st)

insertLabel :: Temp.Label -> G.Node -> State I2gState ()
insertLabel lab node = state $ \st ->
  let
    m = labs st
    m' = Map.insert lab node m
  in
   ((), st{labs=m'})

set_def :: G.Node -> [Int] -> State I2gState ()
set_def node ts = state $ \st ->
  let 
    fg = flowg st
    def = get_def fg
    def' = Map.insert node (filter (0 <) ts) def
    fg' = fg{get_def=def'}
  in
   ((), st{flowg=fg'})
    
set_use :: G.Node -> [Int] -> State I2gState ()
set_use node ts = state $ \st ->
  let 
    fg = flowg st
    use = get_use fg
    use' = Map.insert node (filter (0 <) ts) use
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

insert_edge :: I2gState -> G.Node -> G.Node -> I2gState
insert_edge st n0 n1 =
  let
    fg = flowg st
    g = get_control fg
    g' = G.mk_edge g n0 n1
    fg' = fg{get_control=g'}
  in
   st{flowg=fg'}

pend_edge :: I2gState -> (NodeRef, NodeRef) -> I2gState
pend_edge st x =
  let
    ps = pends st
  in
   st{pends=(x:ps)}

solve_labeled_edges :: Temp.Label -> G.Node -> State I2gState ()
solve_labeled_edges lab node = state $ \st ->
  let
    ps = pends st
    st' = st{pends=[]}

    loop_pends s [] = s
    loop_pends s (x:xs) =
      case x of
        (Known node0, Labeled n) ->
          if n == lab
          then loop_pends (insert_edge s  node0 node) xs
          else loop_pends (pend_edge s x) xs
        _ -> loop_pends (pend_edge s x) xs
  in
   ((), loop_pends st' ps)

add_edge :: NodeRef -> NodeRef -> State I2gState()
add_edge node0@(Known n0) node1 = state $ \st ->
  let
    ps = pends st
    m = labs st
  in
   case node1 of
     NextInstr -> ((), st{pends=(node0, node1):ps})
     Labeled lab ->
       case Map.lookup lab m of
         Just n -> ((), insert_edge st n0 n)
         Nothing -> ((), st{pends=(node0, node1):ps})
     _ -> error "must not occure."

add_edge _ _ = error "edge to add must start from a known node."
   
trInsts :: [A.Instr] -> State I2gState ()

trInsts [] = return ()

trInsts (A.LABEL{A.lab_lab=lab}:insts) =
  do
    node <- newNode
    add_edge (Known node) NextInstr
    insertLabel lab node
    solve_labeled_edges lab node
    set_def node []
    set_use node []
    set_ismove node False
    trInsts insts

trInsts(A.OPER{A.oper_dst=dst, A.oper_src=src, A.oper_jump=js}:insts) =
  do
    node <- newNode
    set_def node dst
    set_use node src
    set_ismove node False
    case js of
      Just (lab:_:[]) ->
        do add_edge (Known node) (Labeled lab)
           add_edge (Known node) NextInstr
      Just (lab:[]) -> add_edge (Known node) (Labeled lab)
      _ -> add_edge (Known node) NextInstr
    trInsts insts

trInsts(A.MOVE{A.move_dst=d, A.move_src=s}:insts) =
  do
    node <- newNode
    add_edge (Known node) NextInstr
    set_def node [d]
    set_use node [s]
    set_ismove node True
    trInsts insts
