module Assem where

import qualified Temp

type Label = Temp.Label

data Instr = OPER { oper_assem :: [String] -> [String] -> [String] -> String
                  , oper_dst :: [Int]
                  , oper_src :: [Int]
                  , oper_jump :: Maybe [Label]
                  }
           | LABEL { lab_assem :: Label -> String
                   , lab_lab :: Label
                   }
           | MOVE { move_assem :: [String] -> [String] -> [String] -> String
                  , move_dst :: Int
                  , move_src :: Int
                  }

instance Show Instr where
  show OPER{oper_assem=assem, oper_dst=ds, oper_src=ss, oper_jump=jmp} =
    let
      ns = [0..] :: [Int]
    in
     "OPER{oper_assem=\"" 
     ++ (assem 
         (fmap (("`d"++).show) ns) 
         (fmap (("`s"++).show) ns)
         (fmap (("`j"++).show) ns))
     ++ "\", oper_dst=" ++ (show ds)
     ++ ", oper_src=" ++ (show ss)
     ++ ", oper_jump=" ++ (show jmp)
     ++ "}"

format saytemp =
  let
    format' OPER{oper_assem=assem, oper_dst=ds, oper_src=ss, oper_jump=jmp} =
      let
        ds' = fmap saytemp ds
        ss' = fmap saytemp ss
        j' = case jmp of
          Just lab -> lab
          _ -> [Temp.namedLabel "(no label)"]
      in
       assem ds' ss' j'
  in
   format'

addInstr :: [Int] -> [Int] -> Instr
addInstr dst src =
  let
    assem ds ss _ =
      "add-int " ++ (ds!!0) ++ ", " ++ (ss!!0) ++ ", " ++ (ss!!1)
  in
   OPER { oper_assem = assem
        , oper_dst = dst
        , oper_src = src
        , oper_jump = Nothing
        }

                    
