module OneSpec (spec) where

import Test.Hspec
import One

input = [ 1721 , 979 , 366 , 299 , 675 , 1456]

spec :: Spec
spec = do
  describe "partOne" $ do
    it "fixes expense reports" $ do
      partOne input `shouldBe` 514579

    it "fixes expense reports, doubly so" $ do
      partTwo input `shouldBe` 241861950
