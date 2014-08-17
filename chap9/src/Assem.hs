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

format :: (Int -> String) -> Instr -> String
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

binOper :: String -> [Int] -> [Int] -> Instr
binOper binop dst src =
  let
    assem ds ss _ =
      binop ++ " " ++ (ds!!0) ++ ", " ++ (ss!!0) ++ ", " ++ (ss!!1)
  in
   OPER { oper_assem = assem
        , oper_dst = dst
        , oper_src = src
        , oper_jump = Nothing
        }

addInstr :: [Int] -> [Int] -> Instr
addInstr = binOper "add-int"

subInstr :: [Int] -> [Int] -> Instr
subInstr = binOper "sub-int"

mulInstr :: [Int] -> [Int] -> Instr
mulInstr = binOper "mul-int"

divInstr :: [Int] -> [Int] -> Instr
divInstr = binOper "div-int"

constInstr :: Int -> Int -> Instr
constInstr c dst =
  let
    assem ds _ _ =
      "const " ++ (ds!!0) ++ ", #+" ++ (show c)
      -- TODO: the immediate value should be hex?
  in
   OPER { oper_assem = assem
        , oper_dst = [dst]
        , oper_src = []
        , oper_jump = Nothing
        }

    
