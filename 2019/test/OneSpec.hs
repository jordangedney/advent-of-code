module OneSpec (spec) where

import Test.Hspec
import One

spec :: Spec
spec = do
  describe "partOne" $
    it "calculates fuel required" $ do
      fuelForModule 12 `shouldBe` 2
      fuelForModule 14 `shouldBe` 2
      fuelForModule 1969 `shouldBe` 654
      fuelForModule 100756 `shouldBe` 33583

  describe "partTwo" $
    it "calculates fuel required while considering the fuel weight" $ do
      totalFuel 14 `shouldBe` 2
      totalFuel 1969 `shouldBe` 966
      totalFuel 100756 `shouldBe` 50346
