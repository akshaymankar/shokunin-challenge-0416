module MagicSquareSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import Lib

spec :: Spec
spec = do
  describe "normal magic square" $ do
    it "should be able to make magic square of size 1" $
      normalMagicSquare 1 `shouldBe` Just [[1]]

    it "should be able to make magic square of size 3" $
      normalMagicSquare 3 `shouldBe` Just [[2,7,6],[9,5,1],[4,3,8]]

  describe "magic sqare" $ do
    it "should be able to make magic square of [1]" $
      magicSquare 1 [1] `shouldBe` Just [[1]]

    it "should be able to make kubera-kolam" $
      magicSquare 3 [23,28,21,22,24,26,27,20,25] `shouldBe` Just [[23,28,21],[22,24,26],[27,20,25]]
