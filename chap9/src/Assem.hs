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
      let
        s1 = "check-cast " ++ (ss!!0) ++ ", Ljava/lang/Integer;\n"
        s2 = "check-cast " ++ (ss!!1) ++ ", Ljava/lang/Integer;\n"
        s3 = "invoke-virtual {" ++ (ss!!0) ++ "}, Ljava/lang/Integer;.intValue:()I\n"
        s4 = "move-result " ++ (ds!!1) ++ "\n"
        s5 = "invoke-virtual {" ++ (ss!!1) ++ "}, Ljava/lang/Integer;.intValue:()I\n"
        s6 = "move-result " ++ (ds!!2) ++ "\n"
        s7 = binop ++ " " ++ (ds!!1) ++ ", " ++ (ds!!1) ++ ", " ++ (ds!!2) ++ "\n"
        s8 = "new-instance " ++ (ds!!0) ++ ", Ljava/lang/Integer;\n"
        s9 = "invoke-direct {" ++ (ds!!0) ++ ", " ++ (ds!!1) ++ ", Ljava/lang/Integer;.<init>:(I)V"
      in
       concat [s1, s2, s3, s4, s5, s6, s7, s8, s9]
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

constInstr :: Int -> [Int] -> Instr
constInstr c dst =
  let
    assem ds _ _ =
      let
        i1 = "const " ++ (ds!!1) ++ ", #int " ++ (show c)
        i2 = "new-instance " ++ (ds!!0) ++ ", Ljava/lang/Integer;"
        i3 = "invoke-direct {" ++ (ds!!0) ++ ", " ++ (ds!!1) 
             ++ "}, Ljava/lang/Integer;.<init>:(I)V"
      in
       concat [i1, "\n", i2, "\n", i3]
  in
   OPER { oper_assem = assem
        , oper_dst = dst
        , oper_src = []
        , oper_jump = Nothing
        }

    
