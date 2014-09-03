import Test.Hspec
import Test.QuickCheck

import qualified DalvikFrame as F
import qualified Translate as TL
import qualified Tree as T
import qualified Temp

eq_test :: IO ()
eq_test = 
  let
    temp = Temp.create
    (lev1, temp') = TL.newLevel TL.outermost (Temp.namedLabel "") [] temp
    (lev2, temp'') = TL.newLevel TL.outermost (Temp.namedLabel "") [] temp'
  in
   hspec $ do
     describe "instance Eq Level" $ do
       it "compares the same levels" $ do
         lev1 == lev1 `shouldBe` True
      
       it "compares outermost and outermost" $ do
         TL.outermost == TL.outermost `shouldBe` True
      
       it "compares outermost and some level" $ do
         TL.outermost == lev1 `shouldBe` False
  
       it "compares some level and outermost" $ do
         lev2 == TL.outermost `shouldBe` False
      
       it "compares two different levels" $ do
         lev1 == lev2 `shouldBe` False
         
       it "is the `/=` operator" $ do
         lev1 /= lev2 `shouldBe` True

simplevar_test :: IO ()
simplevar_test =
  let
    temp = Temp.create
    label1 = Temp.namedLabel "name1"
    label2 = Temp.namedLabel "name2"
    (lev1, temp') = TL.newLevel TL.outermost label1 [] temp
    (lev2, temp'') = TL.newLevel lev1 label2 [] temp'
    
    acc1 = TL.Access{TL.level=lev1, TL.access=F.InFrame 1}
    acc2 = TL.Access{TL.level=lev1, TL.access=F.InReg 2}
    acc3 = TL.Access{TL.level=TL.outermost, TL.access=F.InFrame 3}
    
    var1 = -- F.InFrame 1 from the same level
      case TL.simpleVar acc1 lev1 of TL.Ex e -> e
    
    fp1 = F.fp $ TL.frame lev1
    expected1 = T.MEM $ T.BINOP T.PLUS (T.CONST 1) (T.TEMP fp1)
      
    var2 = -- F.InReg 2
      case TL.simpleVar acc2 lev2 of TL.Ex e -> e
                                     
    var3 = -- F.InFrame 1 from nested level
      case TL.simpleVar acc1 lev2 of TL.Ex e -> e
                                     
    fp2 = F.fp $ TL.frame lev2
    sl2 = F.static_link $ T.TEMP fp2
    expected3 = T.MEM $ T.BINOP T.PLUS (T.CONST 1) sl2
    
    var4 = -- F.InFrame 3 from nester^2 level
      case TL.simpleVar acc3 lev2 of TL.Ex e -> e
                                     
    sl21 = F.static_link sl2
    expected4 = T.MEM $ T.BINOP T.PLUS (T.CONST 3) sl21
  in
   hspec $ do
     describe "simpleVar" $ do
       it "translates simple var InFrame 1 from the same level" $ do
         var1 `shouldBe` expected1

       it "translates simple var InReg 2" $ do
         var2 `shouldBe` T.TEMP 2

       it "translates simple var InFrame 1 from nested level" $ do
         var3 `shouldBe` expected3

       it "translates simple var InFrame 3 from nested^2 level" $ do
         var4 `shouldBe` expected4

main :: IO ()
main = do
  eq_test
  simplevar_test