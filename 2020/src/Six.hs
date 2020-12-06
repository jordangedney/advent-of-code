-- | Finally, my time to shine! [... Damn, Jared was still quicker.]

module Six where

import Util

main :: IO ()
main = do
  input <- map lines . splitOn "\n\n" <$> readFile "inputs/six"
  print (partOne input)
  print (partTwo input)

partOne input = input & map (foldr1 (++)) & map unique & map length & foldr1 (+)

partTwo input =
   length [oneAnswer | groupsAnswers <- input, oneAnswer <- (head groupsAnswer),
           (all (== True) (map (elem oneAnswer) groupsAnswers))]
