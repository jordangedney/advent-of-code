module ThreeSpec (spec) where

import Test.Hspec
import Three

spec :: Spec
spec =
  let inputOne = parse <$> ["R75,D30,R83,U83,L12,D49,R71,U7,L72",
                            "U62,R66,U55,R34,D71,R55,D58,R83"]
      inputTwo = parse <$> ["R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51",
                            "U98,R91,D20,R16,D67,R40,U7,R15,U6,R7"]
  in do describe "partOne" $
          it "find the intersection point closest to the central port" $ do
            partOne inputOne `shouldBe` 159
            partOne inputTwo `shouldBe` 135

        describe "partTwo" $
          it "minimize signal delay" $ do
            partTwo inputOne `shouldBe` 610
            partTwo inputTwo `shouldBe` 410
