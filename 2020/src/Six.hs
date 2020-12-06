-- | Finally, my time to shine! [... Damn, Jared was still quicker.]

module Six where

import Util

main :: IO ()
main = do
  input <- map lines . splitOn "\n\n" <$> readFile "inputs/six"
  print (partOne input)
  print (partTwo input)

partOne input = map (length . unique . concat) input & sum

partTwo input =
   length [oneAnswer | groupsAnswers <- input, oneAnswer <- (head groupsAnswers),
           (all (== True) (map (elem oneAnswer) groupsAnswers))]
