import Test.Hspec
import Test.QuickCheck

import Semant
import qualified Env as E
import qualified Absyn as A
import qualified Translate as TL
import qualified Tree
import qualified Types
import qualified Temp

trprog' venv tenv prog = 
  let
    temp = Temp.create
    (mainlevel, temp') = 
      TL.newLevel TL.outermost (Temp.namedLabel "main") [] temp
    errdest = Temp.namedLabel "_CanNotBreak_"
  in
   case transExp venv tenv errdest mainlevel [] temp' prog of
     (expty, _, _, _) -> expty

nil_test :: IO ()
nil_test =
  let
    venv = E.base_venv
    tenv = E.base_tenv
    ExpTy{expr=expr, ty=t} = trprog' venv tenv A.NilExp
    e = case expr of TL.Ex e -> e
  in
   hspec $ do
     describe "A.NixExp" $ do
       it "returns Tree node for nil" $ do
         e `shouldBe` Tree.CONST 0 
       it "returns T.NIL" $ do
         t `shouldBe` Types.NIL

int_test :: IO ()
int_test =
  let
    venv = E.base_venv
    tenv = E.base_tenv
    expty i = trprog' venv tenv (A.IntExp i undefined)
    expr i = case expty i of ExpTy{expr=TL.Ex e} -> e
    ty i = case expty i of ExpTy{ty=t} -> t
  in      
   hspec $ do
     describe "A.IntExp" $ do
       it "returns Tree node for Int i" $ do
         property $ \i -> 
           expr i `shouldBe` Tree.CONST i
       it "returns T.INT" $ do
         property $ \i -> 
           ty i `shouldBe` Types.INT
    
arith_test :: IO ()
arith_test =
  let
    venv = E.base_venv
    tenv = E.base_tenv
    
    expr expty = case expty of ExpTy{expr=TL.Ex e} -> e
    cond expty = case expty of ExpTy{expr=TL.Cx f} -> f
    
    ty expty = case expty of ExpTy{ty=t} -> t
    
    expty1 = trprog' venv tenv A.OpExp{ A.oper=A.PlusOp
                                        , A.lhs=A.IntExp 1 undefined
                                        , A.rhs=A.IntExp 2 undefined
                                        , A.pos=undefined}
    expty2 = trprog' venv tenv A.OpExp{ A.oper=A.MinusOp
                                        , A.lhs=A.IntExp 3 undefined
                                        , A.rhs=A.IntExp 4 undefined
                                        , A.pos=undefined}
    expty3 = trprog' venv tenv A.OpExp{ A.oper=A.TimesOp
                                        , A.lhs=A.IntExp 5 undefined
                                        , A.rhs=A.IntExp 6 undefined
                                        , A.pos=undefined}
    expty4 = trprog' venv tenv A.OpExp{ A.oper=A.DivideOp
                                        , A.lhs=A.IntExp 7 undefined
                                        , A.rhs=A.IntExp 8 undefined
                                        , A.pos=undefined}
  
    expty5 = trprog' venv tenv A.OpExp{ A.oper=A.LtOp
                                        , A.lhs=A.IntExp 9 undefined
                                        , A.rhs=A.IntExp 10 undefined
                                        , A.pos=undefined}
    expty6 = trprog' venv tenv A.OpExp{ A.oper=A.GtOp
                                        , A.lhs=A.IntExp 11 undefined
                                        , A.rhs=A.IntExp 12 undefined
                                        , A.pos=undefined}
    expty7 = trprog' venv tenv A.OpExp{ A.oper=A.LeOp
                                        , A.lhs=A.IntExp 13 undefined
                                        , A.rhs=A.IntExp 14 undefined
                                        , A.pos=undefined}
    expty8 = trprog' venv tenv A.OpExp{ A.oper=A.GeOp
                                        , A.lhs=A.IntExp 15 undefined
                                        , A.rhs=A.IntExp 16 undefined
                                        , A.pos=undefined}
    expty9 = trprog' venv tenv A.OpExp{ A.oper=A.EqOp
                                        , A.lhs=A.IntExp 17 undefined
                                        , A.rhs=A.IntExp 18 undefined
                                        , A.pos=undefined}
    expty10 = trprog' venv tenv A.OpExp{ A.oper=A.NeqOp
                                         , A.lhs=A.IntExp 19 undefined
                                         , A.rhs=A.IntExp 20 undefined
                                         , A.pos=undefined}
  in 
   hspec $ do
     describe "Binary Operator exps" $ do
       it "returns 1 + 2" $ do
         expr expty1 `shouldBe` Tree.BINOP Tree.PLUS (Tree.CONST 1) (Tree.CONST 2)
       it "returns T.INT" $ do
         ty expty1 `shouldBe` Types.INT
       it "returns 3 - 4" $ do
         expr expty2 `shouldBe` Tree.BINOP Tree.MINUS (Tree.CONST 3) (Tree.CONST 4)
       it "returns T.INT" $ do
         ty expty2 `shouldBe` Types.INT
       it "returns 5 * 6" $ do
         expr expty3 `shouldBe` Tree.BINOP Tree.MUL (Tree.CONST 5) (Tree.CONST 6)
       it "returns T.INT" $ do
         ty expty3 `shouldBe` Types.INT
       it "returns 7 / 8" $ do
         expr expty4 `shouldBe` Tree.BINOP Tree.DIV (Tree.CONST 7) (Tree.CONST 8)
       it "returns T.INT" $ do
         ty expty4 `shouldBe` Types.INT

     describe "Relational Operator exps" $ do
       it "returns CJUMP LT (CONST 9) (CONST 10) t f" $ do
         cond expty5 "t" "f" `shouldBe` 
           Tree.CJUMP Tree.LT (Tree.CONST 9) (Tree.CONST 10) "t" "f"
       it "return INT" $ do ty expty5 `shouldBe` Types.INT
       it "returns CJUMP GT (CONST 11) (CONST 12) t f" $ do
         cond expty6 "t" "f" `shouldBe` 
           Tree.CJUMP Tree.GT (Tree.CONST 11) (Tree.CONST 12) "t" "f"
       it "return INT" $ do ty expty6 `shouldBe` Types.INT
       it "returns CJUMP LE (CONST 13) (CONST 14) t f" $ do
         cond expty7 "t" "f" `shouldBe` 
           Tree.CJUMP Tree.LE (Tree.CONST 13) (Tree.CONST 14) "t" "f"
       it "return INT" $ do ty expty7 `shouldBe` Types.INT
       it "returns CJUMP GE (CONST 15) (CONST 16) t f" $ do
         cond expty8 "t" "f" `shouldBe` 
           Tree.CJUMP Tree.GE (Tree.CONST 15) (Tree.CONST 16) "t" "f"
       it "return INT" $ do ty expty8 `shouldBe` Types.INT
       it "returns CJUMP EQ (CONST 17) (CONST 18) t f" $ do
         cond expty9 "t" "f" `shouldBe` 
           Tree.CJUMP Tree.EQ (Tree.CONST 17) (Tree.CONST 18) "t" "f"
       it "return INT" $ do ty expty9 `shouldBe` Types.INT
       it "returns CJUMP NE (CONST 19) (CONST 20) t f" $ do
         cond expty10 "t" "f" `shouldBe` 
           Tree.CJUMP Tree.NE (Tree.CONST 19) (Tree.CONST 20) "t" "f"
       it "return INT" $ do ty expty10 `shouldBe` Types.INT

if_test :: IO ()
if_test =
  let
    pos=A.Pos{A.line=0,A.column=0}
    
    venv = E.base_venv
    tenv = E.base_tenv
    
    expr expty = case expty of ExpTy{expr=TL.Ex e} -> e
    ty expty = case expty of ExpTy{ty=t} -> t
                             
    expty1 = trprog' venv tenv A.IfExp { A.test=A.IntExp 1 pos
                                         , A.thene=A.IntExp 2 pos
                                         , A.elsee=Just $ A.IntExp 3 pos
                                         , A.pos=pos}
             
    expected1 =  Tree.ESEQ 
                 (Tree.SEQ (Tree.JUMP (Tree.NAME "L0") ["L0"]) 
                  (Tree.SEQ (Tree.LABEL "L0")
                   (Tree.SEQ (Tree.MOVE (Tree.TEMP 1) (Tree.CONST 2)) 
                    (Tree.SEQ (Tree.JUMP (Tree.NAME "L2") ["L2"]) 
                     (Tree.SEQ (Tree.LABEL "L1") 
                      (Tree.SEQ (Tree.MOVE (Tree.TEMP 1) (Tree.CONST 3)) 
                       (Tree.LABEL "L2"))))))) 
                 (Tree.TEMP 1)

    expty2 = trprog' venv tenv A.IfExp { A.test=A.IntExp 0 pos
                                         , A.thene=A.IntExp 4 pos
                                         , A.elsee=Just $ A.IntExp 5 pos
                                         , A.pos=pos}
             
    expected2 =  Tree.ESEQ 
                 (Tree.SEQ (Tree.JUMP (Tree.NAME "L1") ["L1"]) 
                  (Tree.SEQ (Tree.LABEL "L0")
                   (Tree.SEQ (Tree.MOVE (Tree.TEMP 1) (Tree.CONST 4)) 
                    (Tree.SEQ (Tree.JUMP (Tree.NAME "L2") ["L2"]) 
                     (Tree.SEQ (Tree.LABEL "L1") 
                      (Tree.SEQ (Tree.MOVE (Tree.TEMP 1) (Tree.CONST 5)) 
                       (Tree.LABEL "L2"))))))) 
                 (Tree.TEMP 1)
                 
    expty3 = trprog' venv tenv A.IfExp { A.test=A.OpExp{A.oper=A.EqOp
                                                         ,A.lhs=A.IntExp 6 pos
                                                         ,A.rhs=A.IntExp 7 pos
                                                         ,A.pos=pos}
                                         , A.thene=A.IntExp 8 pos
                                         , A.elsee=Just $ A.IntExp 9 pos
                                         , A.pos=pos}
    expected3 =  Tree.ESEQ 
                 (Tree.SEQ (Tree.CJUMP 
                            Tree.EQ (Tree.CONST 6)(Tree.CONST 7) "L0" "L1") 
                  (Tree.SEQ (Tree.LABEL "L0") 
                   (Tree.SEQ (Tree.MOVE (Tree.TEMP 1) (Tree.CONST 8)) 
                    (Tree.SEQ (Tree.JUMP (Tree.NAME "L2") ["L2"]) 
                     (Tree.SEQ (Tree.LABEL "L1") 
                      (Tree.SEQ (Tree.MOVE (Tree.TEMP 1) (Tree.CONST 9)) 
                       (Tree.LABEL "L2")))))))
                 (Tree.TEMP 1)
    
    expty4 = trprog' venv tenv A.IfExp { A.test=A.OpExp{A.oper=A.EqOp
                                                         ,A.lhs=A.IntExp 10 pos
                                                         ,A.rhs=A.IntExp 11 pos
                                                         ,A.pos=pos}
                                         , A.thene=A.SeqExp []
                                         , A.elsee=Nothing
                                         , A.pos=pos}
    expected4 = Tree.ESEQ 
                (Tree.SEQ (Tree.CJUMP Tree.EQ (Tree.CONST 10) (Tree.CONST 11) "L0" "L1") 
                 (Tree.SEQ (Tree.LABEL "L0")
                  (Tree.SEQ (Tree.EXP (Tree.CONST 0)) (Tree.LABEL "L1")))) 
                (Tree.CONST 0)
      
  in
   hspec $ do
     describe "IfThenElse" $ do
       it "returns always true pattern" $ do
         expr expty1 `shouldBe` expected1
         ty expty1 `shouldBe` Types.INT
       it "returns always false pattern" $ do
         expr expty2 `shouldBe` expected2
         ty expty2 `shouldBe` Types.INT
       it "returns if-then-else" $ do
         expr expty3 `shouldBe` expected3
         ty expty3 `shouldBe` Types.INT
       it "returns if-then" $ do
         expr expty4 `shouldBe` expected4
         ty expty4 `shouldBe` Types.UNIT
                                               
main :: IO ()
main = do
  nil_test
  int_test
  arith_test
  if_test
  