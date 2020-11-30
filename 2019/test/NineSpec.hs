module NineSpec (spec) where

import Test.Hspec
import Nine
import Util (digits)

import qualified Data.Map.Strict as Map

spec :: Spec
spec =
  describe "partOne" $ do
    it "takes no input and produces a copy of itself as output." $ do
      let prog = [109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99]
          input = Map.fromList $ zip [0..] prog
      runIntcodeProgram [] input `shouldBe` prog

    it "should output a 16-digit number." $ do
      let prog = [1102,34915192,34915192,7,4,7,99,0]
          input = Map.fromList $ zip [0..] prog
      (length . digits $ head (runIntcodeProgram [] input)) `shouldBe` 16

    it "should output the large number in the middle." $ do
      let prog = [104,1125899906842624,99]
          input = Map.fromList $ zip [0..] prog
      head (runIntcodeProgram [] input) `shouldBe` 1125899906842624
