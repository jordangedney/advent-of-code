module Ten where

import Util (Coord, manhattan, sort, tracer)

toCoords :: [String] -> [Coord]
toCoords input = [(x, y) | (y, xs) <- zip [0..] input, (x, a) <- zip [0..] xs, a == '#']

genLine :: Coord -> Coord -> Coord -> [Coord]
genLine (maxX, maxY) curAstroid other =
  let distanceToAstroid (x, y) (a, b) = (a - x, b - y)
      reduceVector (c, d) = (c `div` gcd c d, d `div` gcd c d)
      (a, b) = reduceVector $ distanceToAstroid curAstroid other
      withinBounds (x, y)
        | x < 0 || y < 0 || x > maxX || y > maxY = False
        | otherwise = True
  in takeWhile withinBounds $ iterate (\(c, d) -> (c + a, d + b)) other

withVisibleAstroids :: [Coord] -> [(Coord, [Coord])]
withVisibleAstroids allAstroids =
  let max = (maximum $ map fst allAstroids, maximum $ map snd allAstroids)
      getVisible a = visibleAstroids max a (otherAstroidsByDistance a allAstroids)
  in map (\x -> (x, getVisible x)) allAstroids

visibleAstroids :: Coord -> Coord -> [Coord] -> [Coord]
visibleAstroids max coord otherCoords = go [] otherCoords
  where go blockedCoords [] = []
        go blockedCoords (astroid:astroids) =
          let unavailableCoords = genLine max coord astroid ++ blockedCoords
          in astroid : go unavailableCoords (filter (`notElem` unavailableCoords) astroids)

otherAstroidsByDistance :: Coord -> [Coord] -> [Coord]
otherAstroidsByDistance start =
  map snd . sort . map (\x -> (manhattan start x, x)) . filter (/= start)

partTwo :: [Coord] -> [Coord]
partTwo input =
  let (_, start) =
        head $ reverse $ sort $ map (\(x, y) -> (length y, x)) $ withVisibleAstroids input
      max = (maximum $ map fst input, maximum $ map snd input)
      getVisible a allAstroids =
        visibleAstroids max a (otherAstroidsByDistance a allAstroids)

      go [] = []
      go astroids =
        let visible = getVisible start astroids
            toBlowUp = clockwiseSort start visible
        in toBlowUp ++ go (filter (`notElem` toBlowUp) astroids)

  in go (filter (/= start) input)

clockwiseSort :: Coord -> [Coord] -> [Coord]
clockwiseSort start@(x, y) xs =
  let sortByAtan = sort . map (\(a, b) -> (atan2 (fromIntegral a) (fromIntegral b), (a, b)))
      translateRelative (x, y) = map (\(a, b) -> (a - x, b - y))
  in reverse $ translateRelative (-x, -y) $ map snd $ sortByAtan $ translateRelative start xs

progMain :: IO ()
progMain = do
  input <- toCoords . lines <$> readFile "inputs/ten"
  print $ maximum $ map (length . snd) $ withVisibleAstroids input
  let (x, y) = partTwo input !! 199
  print $ x * 100 + y
