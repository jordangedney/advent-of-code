module ThreeSpec (spec) where

import Test.Hspec
import Three

input' = [ "..##......."
         , "#...#...#.."
         , ".#....#..#."
         , "..#.#...#.#"
         , ".#...##..#."
         , "..#.##....."
         , ".#.#.#....#"
         , ".#........#"
         , "#.##...#..."
         , "#...##....#"
         , ".#..#...#.#"
         ]
input = map parseArea input'

spec :: Spec
spec = do
  describe "partOne" $ do
    it "counts passing trees" $ do
      partOne input `shouldBe` 7
