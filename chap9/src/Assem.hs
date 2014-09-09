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

check_cast :: String -> String -> String
check_cast r s =
  "check-cast " ++ r ++ ", " ++ s ++ "\n"

integer2int :: String -> String -> String
integer2int s d =
  "invoke-virtual {" ++ s ++ "}, Ljava/lang/Integer;.intValue:()I\n"
  ++ "move-result " ++ d ++ "\n"
  
int2integer :: String -> String -> String
int2integer s d =  
  "new-instance " ++ d ++ ", Ljava/lang/Integer;\n"
  ++ "invoke-direct {" ++ d ++ ", " ++ s ++ ", Ljava/lang/Integer;.<init>:(I)V"

binOper :: String -> [Int] -> [Int] -> Instr
binOper binop dst src =
  let
    assem ds ss _ =
      let
        s1 = check_cast (ss!!0) "Ljava/lang/Integer;"
        s2 = check_cast (ss!!1) "Ljava/lang/Integer;"
        s3 = integer2int (ss!!0) (ds!!1)
        s4 = integer2int (ss!!1) (ds!!2)
        s5 = binop ++ " " ++ (ds!!1) ++ ", " ++ (ds!!1) ++ ", " ++ (ds!!2) ++ "\n"
        s6 = int2integer (ds!!1) (ds!!0)
      in
       concat [s1, s2, s3, s4, s5, s6]
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

memInstr :: [Int] -> [Int] -> Instr
memInstr dst src =
  let 
    assem ds ss _ =
      let
        s1 = check_cast (ss!!1) "Ljava/lang/Integer;"
        s2 = integer2int (ss!!1) (ds!!1)
        s3 = check_cast (ss!!0) "[Ljava/lang/Object;"
        s4 = "aget-object " ++ (ds!!0) ++ ", " ++ (ss!!0) ++ ", " ++ (ds!!1) ++ "\n"
      in
       concat [s1, s2, s3, s4]
  in
   OPER { oper_assem = assem
        , oper_dst = dst
        , oper_src = src
        , oper_jump = Nothing
        }

strInstr :: String -> [Int] -> Instr
strInstr s dst =
  let
    assem ds _ _ =
      "const-string " ++ (ds!!0) ++ (show s)
  in
   OPER { oper_assem = assem
        , oper_dst = dst
        , oper_src = []
        , oper_jump = Nothing
        }

moveInstr :: [Int] -> [Int] -> Instr
moveInstr dst src =
  let
    assem ds ss _ =
      let
        s1 = check_cast (ss!!0) "L/java/lang/Integer;"
        s2 = integer2int (ss!!0) (ds!!1)
        s3 = "aput-object " ++ (ss!!1) ++ ", " ++ (ds!!0) ++ ", " ++ (ds!!1) ++ "\n"
      in
       concat [s1, s2, s3]
  in
   OPER { oper_assem = assem
        , oper_dst = dst
        , oper_src = src
        , oper_jump = Nothing
        }

cjumpInstr :: String -> [Int] -> [Int] -> Label -> Instr
cjumpInstr cond dst src label =
  let
    assem ds ss _ =
      let
        s1 = check_cast (ss!!0) "L/java/lang/Integer;"
        s2 = integer2int (ss!!0) (ds!!0)
        s3 = check_cast (ss!!1) "L/java/lang/Integer;"
        s4 = integer2int (ss!!1) (ds!!1)
        s5 = "if-" ++ cond ++ " " ++ (ds!!0) ++ ", " ++ (ds!!1) ++ ", :" ++ label ++ "\n"
      in
       concat [s1, s2, s3, s4, s5]
  in
   OPER { oper_assem = assem
        , oper_dst = dst
        , oper_src = src
        , oper_jump = Just [label]
        }

jumpInstr :: Label -> Instr
jumpInstr label =
  let
    assem _ _ _ = "goto :" ++ label ++ "\n"
  in
   OPER { oper_assem = assem
        , oper_dst = []
        , oper_src = []
        , oper_jump = Just [label]
        }

