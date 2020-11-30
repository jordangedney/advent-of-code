module TenSpec (spec) where

import Test.Hspec
import Ten

-- Best is 5,8 with 33 other asteroids detected:
input1 = toCoords [ "......#.#."
                  , "#..#.#...."
                  , "..#######."
                  , ".#.#.###.."
                  , ".#..#....."
                  , "..#....#.#"
                  , "#..#....#."
                  , ".##.#..###"
                  , "##...#..#."
                  , ".#....####"
                  ]

-- Best is 1,2 with 35 other asteroids detected:
input2 = toCoords [ "#.#...#.#."
                  , ".###....#."
                  , ".#....#..."
                  , "##.#.#.#.#"
                  , "....#.#.#."
                  , ".##..###.#"
                  , "..#...##.."
                  , "..##....##"
                  , "......#..."
                  , ".####.###."
                  ]
-- Best is 6,3 with 41 other asteroids detected:
input3 = toCoords [ ".#..#..###"
                  , "####.###.#"
                  , "....###.#."
                  , "..###.##.#"
                  , "##.##.#.#."
                  , "....###..#"
                  , "..#.#..#.#"
                  , "#..#.#.###"
                  , ".##...##.#"
                  , ".....#.#.."
                  ]

-- Best is 11,13 with 210 other asteroids detected:
input4 = toCoords
  [".#..##.###...#######"
  , "##.############..##."
  , ".#.######.########.#"
  , ".###.#######.####.#."
  , "#####.##.#.##.###.##"
  , "..#####..#.#########"
  , "####################"
  , "#.####....###.#.#.##"
  , "##.#################"
  , "#####.##.###..####.."
  , "..######..##.#######"
  , "####.##.####...##..#"
  , ".#####..#.######.###"
  , "##...#.##########..."
  , "#.##########.#######"
  , ".####.#.###.###.#.##"
  , "....##.##.###..#####"
  , ".#.#.###########.###"
  , "#.#.#.#####.####.###"
  , "###.##.####.##.#..##"
  ]

spec :: Spec
spec = do
  describe "partOne" $ do
    it "1: can count astroids" $ do
      (maximum $ map (length . snd) $ withVisibleAstroids input1) `shouldBe` 33

    it "2: can count astroids" $ do
      (maximum $ map (length . snd) $ withVisibleAstroids input2) `shouldBe` 35

    it "3: can count astroids" $ do
      (maximum $ map (length . snd) $ withVisibleAstroids input3) `shouldBe` 41

    it "4: can count astroids" $ do
      (maximum $ map (length . snd) $ withVisibleAstroids input4) `shouldBe` 210

  describe "partTwo" $ do
    it "The 1st   asteroid to be vaporized" $ do
      (partTwo input4) !! 0 `shouldBe` (11,12)

    it "The 2nd   asteroid to be vaporized" $ do
      (partTwo input4) !! 1 `shouldBe` (12,1)

    it "The 3) !! 10rd   asteroid to be vaporized" $ do
      (partTwo input4) !! 2 `shouldBe` (12,2)

    it "The 10th  asteroid to be vaporized" $ do
      (partTwo input4) !! 9 `shouldBe` (12,8)

    it "The 20th  asteroid to be vaporized" $ do
      (partTwo input4) !! 19 `shouldBe` (16,0)

    it "The 50th  asteroid to be vaporized" $ do
      (partTwo input4) !! 49 `shouldBe` (16,9)

    it "The 100th asteroid to be vaporized" $ do
      (partTwo input4) !! 99 `shouldBe` (10,16)

    it "The 199th asteroid to be vaporized" $ do
      (partTwo input4) !! 198 `shouldBe` (9,6)

    it "The 200th asteroid to be vaporized" $ do
      (partTwo input4) !! 199 `shouldBe` (8,2)

    it "The 201st asteroid to be vaporized" $ do
      (partTwo input4) !! 200 `shouldBe` (10,9)

    it "The 299th asteroid to be vaporized" $ do
      (partTwo input4) !! 298 `shouldBe` (11,1)
