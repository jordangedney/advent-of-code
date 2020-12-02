module TwoSpec (spec) where

import Test.Hspec
import Two

input' = ["1-3 a: abcde" , "1-3 b: cdefg" , "2-9 c: ccccccccc"]
input = map mkPasswordEntry input'

spec :: Spec
spec = do
  describe "partOne" $ do
    it "counts valid passwords" $ do
      partOne input `shouldBe` 2

    -- it "fixes expense reports, doubly so" $ do
    --   partTwo input `shouldBe` 241861950
