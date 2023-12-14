import Lude

main :: IO ()
main = do
  let input = testInput
  input <- readFile "inputs/13"
  -- Tried and Failed:
  -- 38286 - too high
  -- 37972 - 

  -- part one
  let parsed = splitOn "\n\n" input & map lines
      zipSplitFlip i xs = ("H", "H") : (drop 1 $ zip (reverse (take i xs)) (drop i xs))
      matches f = [x | (x, (y, z)) <- zip [1..] (zip f (tail f)), z == y]
      reflectP i xs = all (uncurry (==)) (zipSplitFlip i xs)
      getRefl xs = [m | m <- matches xs, reflectP m xs]

  print $ parsed & map (\xs -> (getRefl xs, getRefl $ transpose xs))
                 & map (\(a, b) -> head $ ((100 *) <$> a) ++ b)
                 & sum
  -- part two
  let programmersEqual [] [] = True
      programmersEqual (a:as) (b:bs) | a == b = programmersEqual as bs
                                     | otherwise = as == bs
      programmersEqual _ _ = False

      matches2 f = [(x, z == y) | (x, (y, z)) <- zip [1..] (zip f (tail f)), z `programmersEqual` y]
      -- almostReflectP i xs = all (uncurry (==)) $ zipSplitFlip i xs
      almostReflectP i xs =
        let comparisons = zipSplitFlip i xs
                        & map (\(xs, ys) -> (programmersEqual xs ys, xs == ys))
            tooManySmudges = 
              (filter (\cmp -> cmp == (False, True)) comparisons & length) > 1
            equalEnough = all (uncurry (||)) comparisons
        in not tooManySmudges && equalEnough
        -- in traceShow (comparisons, tooManySmudges, equalEnough) not tooManySmudges && equalEnough

      reflectP2 1 xs = True
      reflectP2 i xs = all (uncurry (==)) (zipSplitFlip i xs)

      getRefl2 xs = [m | (m, eqP) <- matches2 xs,
                         if eqP then almostReflectP m xs
                         else reflectP m xs ]


  mapM_ print $ parsed & map (\xs -> (getRefl2 xs, getRefl2 $ transpose xs))

  print $ parsed & map (\xs -> (getRefl2 xs, getRefl2 $ transpose xs))
                 & map (\(a, b) -> ((100 *) <$> a, b))
                 & map (\(a, b) -> (head $ a ++ [0]) + (head $ b ++ [0]))
                 & sum
  -- print $ matches2  (parsed !! 1)
  -- print $ zipSplitFlip 4  (parsed !! 1)



testInput =
  [ "#.##..##."
  , "..#.##.#."
  , "##......#"
  , "##......#"
  , "..#.##.#."
  , "..##..##."
  , "#.#.##.#."
  , ""
  , "#...##..#"
  , "#....#..#"
  , "..##..###"
  , "#####.##."
  , "#####.##."
  , "..##..###"
  , "#....#..#" ] & unlines