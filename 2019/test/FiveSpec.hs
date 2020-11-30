module FiveSpec (spec) where

import Test.Hspec
import Five

spec :: Spec
spec = do
  describe "partOne" $ do
    it "intcode can multiply numbers" $ do
      let (result, _) = runIntcodeProgram 0 [1002, 4, 3, 4, 33]
      result `shouldBe` [1002, 4, 3, 4, 99]

    it "intcode can subtract numbers" $ do
      let (result, _) = runIntcodeProgram 0 [1101, 100, -1, 4, 0]
      result `shouldBe` [1101, 100, -1, 4, 99]

  describe "partTwo" $ do
    let runProgram p i = head . snd $ runIntcodeProgram i p

    it "(position) is the input is equal to 8; output 1 (if it is) or 0." $ do
      let withInput = runProgram [3, 9, 8, 9, 10, 9, 4, 9, 99, -1, 8]
      withInput 8  `shouldBe` 1
      withInput 10 `shouldBe` 0
      withInput 4  `shouldBe` 0

    it "(position) is the input is less than 8; output 1 (if it is) or 0." $ do
      let withInput = runProgram [3, 9, 7, 9, 10, 9, 4, 9, 99, -1, 8 ]
      withInput 8  `shouldBe` 0
      withInput 10 `shouldBe` 0
      withInput 4  `shouldBe` 1

    it "(immediate) is the input is equal to 8; output 1 (if it is) or 0." $ do
      let withInput = runProgram [3, 3, 1108, -1, 8, 3, 4, 3, 99]
      withInput 8  `shouldBe` 1
      withInput 11 `shouldBe` 0
      withInput 3  `shouldBe` 0

    it "(immediate) is the input is less than 8; output 1 (if it is) or 0." $ do
      let withInput = runProgram [3, 3, 1107, -1, 8, 3, 4, 3, 99]
      withInput 8  `shouldBe` 0
      withInput 11 `shouldBe` 0
      withInput 3  `shouldBe` 1

    it "jump test (position) ; output 0 if the input was zero or 1 otherwise" $ do
      let withInput = runProgram [3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9]
      withInput 1    `shouldBe` 1
      withInput 0    `shouldBe` 0
      withInput (-1) `shouldBe` 1

    it "jump test (immediate) ; output 0 if the input was zero or 1 otherwise" $ do
      let withInput = runProgram [3, 3, 1105, -1, 9, 1101, 0, 0, 12, 4, 12, 99, 1]
      withInput 0    `shouldBe` 0
      withInput 1    `shouldBe` 1
      withInput (-1) `shouldBe` 1

    it "test everything" $ do
      let withInput = runProgram [3, 21, 1008, 21, 8, 20, 1005, 20, 22, 107, 8,
                                  21, 20, 1006, 20, 31, 1106, 0, 36, 98, 0, 0,
                                  1002, 21, 125, 20, 4, 20, 1105, 1, 46, 104,
                                  999, 1105, 1, 46, 1101, 1000, 1, 20, 4, 20,
                                  1105, 1, 46, 98, 99]
      withInput 0   `shouldBe` 999
      withInput 5   `shouldBe` 999
      withInput 8   `shouldBe` 1000
      withInput 9   `shouldBe` 1001
      withInput 100 `shouldBe` 1001
