import Lude

raceData :: Parser ([Int], [Int])
raceData = do
  times     <- string "Time:" >> ws >> some entry <* newline
  distances <- string "Distance:" >> ws >> some entry <* newline
  pure (times, distances)

main :: IO ()
main = do
  let input =
        [ "Time:      7  15   30"
        , "Distance:  9  40  200" ] & unlines
  input <- readFile "inputs/6"

  -- part one
  let [(times, distances)] = [parse raceData "" input] & rights
      possibleDistances totalTime = [t * (totalTime - t) | t <- [0..totalTime]]
      solve ds ts =  zip ds (map possibleDistances ts) 
                     & map (\(m, xs) -> filter (> m ) xs & length)
                     & product
  print $ solve distances times

  -- part two
  let smoosh :: [Int] -> Int
      smoosh xs = concatMap show xs & read
  print $ solve [smoosh distances] [smoosh times] 
