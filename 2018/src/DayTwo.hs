module DayTwo where


  -- Trash
-- numDifChrs' xs ys =
--   foldr (\(x, y) (b, c) -> if x == y then (b, c ++ [x]) else (b ++ [x], c)) ([], []) (zip xs ys)
--
-- partTwo' :: [String] -> String
-- partTwo' (b:boxIds) =
--   case numDifChrs' b b2 of
--     (matching, unmatching) | length unmatching == 1 -> matching
--     otherwise -> partTwo boxIds

  -- Trash

numDifChrs xs ys = length $ filter (uncurry (/=)) $ (zip xs ys)

partTwo :: [String] -> String
partTwo (b:boxIds) =
  case filter (\b2 -> numDifChrs b b2 == 1) boxIds of
    id2:_ -> map fst $ filter (uncurry (==)) $ zip b id2
    [] -> partTwo boxIds

dayTwoMain :: IO ()
dayTwoMain = do
  boxIds <- fmap lines $ readFile "inputs/DayTwoInput"

  let howManyWith x = length $ filter (charWithCountX x) boxIds
      charWithCountX n xs = not . null $ [x | x <- xs, (count x xs) == n]
      count x = length . filter (x ==)

  print $ howManyWith 2 * howManyWith 3

  putStrLn $ show $ partTwo boxIds
