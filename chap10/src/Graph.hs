module Graph where

import qualified Data.Map as Map
import Data.List

newtype Node = Node Int
             deriving (Eq, Ord, Show)

data Graph = Graph { get_nnodes :: Int
                   , get_adjacency :: Map.Map (Int, Int) Bool
                   }
             deriving (Eq, Show)

newGraph :: Graph
newGraph = Graph { get_nnodes = 0
                 , get_adjacency = Map.empty
                 }

newNode :: Graph -> (Node, Graph)
newNode g =
  let
    n = get_nnodes g
  in
   (Node n, g{get_nnodes=(n+1)})

mk_edge :: Graph -> Node -> Node -> Graph
mk_edge g (Node nf) (Node nt) =
  let
    adj = get_adjacency g
    adj' = Map.insert (nf, nt) True adj
  in
   g{get_adjacency=adj'}

rm_edge :: Graph -> Node -> Node -> Graph
rm_edge g (Node nf) (Node nt) =
  let
    adj = get_adjacency g
    adj' = Map.delete (nf, nt) adj
  in
   g{get_adjacency=adj'}
  
nodes :: Graph -> [Node]
nodes g =
  let
    n = get_nnodes g
  in
   [Node x | x <- [0..(n-1)]]

succ_nodes :: Graph -> Node -> [Node]
succ_nodes g (Node x') =
  let
    adj = get_adjacency g
    n = get_nnodes g
    cand = [(x', y) | y <- [0..(n-1)]]
  in
   cand >>= (\(x, y) -> case Map.lookup (x, y) adj of
                Just True -> [Node y]
                _ -> [])

pred_nodes :: Graph -> Node -> [Node]
pred_nodes g (Node y') =
  let
    adj = get_adjacency g
    n = get_nnodes g
    cand = [(x, y') | x <- [0..(n-1)]]
  in
   cand >>= (\(x, y) -> case Map.lookup (x, y) adj of
                Just True -> [Node x]
                _ -> [])

adj_nodes :: Graph -> Node -> [Node]
adj_nodes g node = nub $ succ_nodes g node ++ pred_nodes g node

