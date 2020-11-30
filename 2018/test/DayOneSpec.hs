module DayOneSpec (spec) where

import Test.Hspec
import DayOne

spec :: Spec
spec = do
  describe "partOne" $ do
    it "it sums a list" $ do
      partOne [1, 1, 1] `shouldBe` 3
      partOne [1, 1, -2] `shouldBe` 0
      partOne [-1, -2, -3] `shouldBe` -6

  describe "partTwo" $ do
    it "it finds the first frequency reached twice" $ do
      partTwo [1, -1] `shouldBe` 0
      partTwo [3, 3, 4, -2, -4] `shouldBe` 10
      partTwo [-6, 3, 8, 5, -6] `shouldBe` 5
      partTwo [7, 7, -2, -7, -4] `shouldBe` 14
