module Flow where

import qualified Data.Map as Map

import qualified Graph as G
import qualified Temp

data Flowgraph = FGRAPH { get_control :: G.Graph
                        , get_def :: Map.Map G.Node [Temp.Temp]
                        , get_use :: Map.Map G.Node [Temp.Temp]
                        , get_ismove :: Map.Map G.Node Bool
                        }

