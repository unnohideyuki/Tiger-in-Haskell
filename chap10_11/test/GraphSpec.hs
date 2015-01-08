module Main where

import Test.Hspec

import qualified Graph as G
import qualified Data.Map as Map

graph_test :: IO ()
graph_test =
  let
    eg0 = G.Graph { G.get_nnodes = 0
                  , G.get_adjacency = Map.empty
                  }

    (_, g1) = G.newNode eg0
    (_, g2) = G.newNode g1
    (n3, g3) = G.newNode g2

    eg3 = G.Graph { G.get_nnodes = 3
                  , G.get_adjacency = Map.empty
                  }

    g4 = G.mk_edge g3 (G.Node 0) (G.Node 1)
    g5 = G.mk_edge g4 (G.Node 0) (G.Node 2)
    g6 = G.mk_edge g5 (G.Node 1) (G.Node 0)
    g7 = G.mk_edge g6 (G.Node 2) (G.Node 1)

    eg7 = G.Graph { G.get_nnodes = 3
                  , G.get_adjacency =
                    Map.fromList [((0,1), True)
                                 ,((0,2), True)
                                 ,((1,0), True)
                                 ,((2,1), True)]
                  }

    g8 = G.rm_edge eg7 (G.Node 1) (G.Node 0)

    eg8 = G.Graph { G.get_nnodes = 3
                  , G.get_adjacency =
                    Map.fromList [((0,1), True)
                                 ,((0,2), True)
                                 ,((2,1), True)]
                  }
  in
   hspec $ do
     describe "newGraph" $ do
       it "returns an empty graph " $
         G.newGraph `shouldBe` eg0
       it "returns a graph with 3 nodes." $
         g3 `shouldBe` eg3
       it "returns (Node 2)." $
         n3 `shouldBe` G.Node 2
       it "returns a graph with 3 nodes, 4 edges" $
         g7 `shouldBe` eg7
       it "returns the successors of the Node 0" $
         G.succ_nodes g7 (G.Node 0) `shouldBe` [G.Node 1, G.Node 2]
       it "returns the successors of the Node 1" $
         G.succ_nodes g7 (G.Node 1) `shouldBe` [G.Node 0]
       it "returns the successors of the Node 2" $
         G.succ_nodes g7 (G.Node 2) `shouldBe` [G.Node 1]
       it "returns the predecessors of the Node 0" $
         G.pred_nodes g7 (G.Node 0) `shouldBe` [G.Node 1]
       it "returns the predecessors of the Node 1" $
         G.pred_nodes g7 (G.Node 1) `shouldBe` [G.Node 0, G.Node 2]
       it "returns the predecessors of the Node 2" $
         G.pred_nodes g7 (G.Node 2) `shouldBe` [G.Node 0]
       it "returns the adjacencies of the Node 0" $
         G.adj_nodes g7 (G.Node 0) `shouldBe` [G.Node 1, G.Node 2]
       it "returns the adjacencies of the Node 1" $
         G.adj_nodes g7 (G.Node 1) `shouldBe` [G.Node 0, G.Node 2]
       it "returns the adjacencies of the Node 2" $
         G.adj_nodes g7 (G.Node 2) `shouldBe` [G.Node 1, G.Node 0]
       it "returns a graph with 3 nodes, 3 edges" $
         g8 `shouldBe` eg8
   
main :: IO ()
main = do
  graph_test
  
