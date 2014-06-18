import Test.Hspec
import Test.QuickCheck

import qualified Translate as TL
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
    (lev1, temp') = TL.newLevel TL.outermost (Temp.namedLabel "") [] temp
  in
   -- How to test the result when Ex constructor is not public?
   undefined

main :: IO ()
main = do
  eq_test
  simplevar_test