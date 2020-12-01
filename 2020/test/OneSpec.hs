module OneSpec (spec) where

import Test.Hspec
import One

input = [ 1721
        , 979
        , 366
        , 299
        , 675
        , 1456
        ]

spec :: Spec
spec = do
  describe "partOne" $
    it "fixes expense reports" $ do
      partOne input `shouldBe` 514579
