module TwoSpec (spec) where

import Test.Hspec
import Two

spec :: Spec
spec =
  describe "partOne" $
    it "runs intcode programs" $ do
      head (runIntcodeProgram [1, 9, 10, 3, 2, 3, 11, 0, 99, 30, 40, 50]) `shouldBe` 3500
      runIntcodeProgram [1, 0, 0, 0, 99] `shouldBe` [2, 0, 0, 0, 99]
      runIntcodeProgram [2, 3, 0, 3, 99] `shouldBe` [2, 3, 0, 6, 99]
      runIntcodeProgram [2, 4, 4, 5, 99, 0] `shouldBe` [2, 4, 4, 5, 99, 9801]
      runIntcodeProgram [1, 1, 1, 4, 99, 5, 6, 0, 99] `shouldBe`
        [30, 1, 1, 4, 2, 5, 6, 0, 99]

  -- No tests given for partTwo
