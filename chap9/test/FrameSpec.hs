import Test.Hspec
import Test.QuickCheck

import qualified DalvikFrame as F
import qualified Tree as T
import qualified Temp

exp_test :: IO ()
exp_test =
  let
    temp = Temp.create
    fpexp = T.TEMP 0
  in
   hspec $ do
     describe "Frame.exp" $ do
       it "returns T.TEMP t for F.InReg t" $ property $ \t ->
         F.exp (F.InReg t) fpexp `shouldBe` T.TEMP t
         
       it "returns T.MEM node to access the InFrame variable" $
         property $ \(k, t) ->
         F.exp (F.InFrame k) (T.TEMP t) `shouldBe` 
         (T.MEM $ T.BINOP T.PLUS (T.CONST k) (T.TEMP t))

main :: IO ()
main = do
  exp_test
  

