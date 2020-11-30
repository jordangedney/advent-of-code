module SixSpec (spec) where

import Test.Hspec
import Six
import Util (splitOn)

spec :: Spec
spec = do
  describe "partOne" $ do
    let input = parse $ fmap (splitOn ")")
          ["COM)B", "B)C", "C)D", "D)E", "E)F", "B)G", "G)H", "D)I", "E)J", "J)K", "K)L"]

    it "can count dependencies" $
      partOne input `shouldBe` 42

  describe "partTwo" $ do
    let input = parse $ fmap (splitOn ")")
          ["COM)B", "B)C", "C)D", "D)E", "E)F", "B)G", "G)H", "D)I", "E)J",
          "J)K", "K)L", "K)YOU", "I)SAN"]

    it "can figure out the minimum number of orbital transfers required" $ do
      partTwo input `shouldBe` 4
