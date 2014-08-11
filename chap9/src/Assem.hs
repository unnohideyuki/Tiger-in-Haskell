module Assem where

import qualified Temp

type Label = Temp.Label

data Instr = OPER { oper_assem :: [Int] -> [Int] -> String
                  , oper_dst :: [Int]
                  , oper_src :: [Int]
                  , oper_jump :: Maybe [Label]
                  }
           | LABEL { lab_assem :: Label -> String
                   , lab_lab :: Label
                   }
           | MOVE { move_assem :: Int -> Int -> String
                  , move_dst :: Int
                  , move_src :: Int
                  }

addInstr :: [Int] -> [Int] -> Instr
addInstr dst src =
  let
    assem ds ss = 
      let
        d = show $ ds !! 0
        s1 = show $ ss !! 0
        s2 = show $ ss !! 1
      in
       "add-int  v" ++ d ++ ", v" ++ s1 ++ ", v" ++ s2
  in
   OPER { oper_assem = assem
        , oper_dst = dst
        , oper_src = src
        , oper_jump = Nothing
        }

                    
