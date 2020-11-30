module Four where

import Util (sort, count)

neverDecreases :: String -> Bool
neverDecreases x = x == sort x

progMain :: IO ()
progMain = do
  let input = map show ([272091..815432] :: [Int])
  print $ length [xs | xs <- input, neverDecreases xs, or [True | x <- xs, count x xs >= 2]]
  print $ length [xs | xs <- input, neverDecreases xs, or [True | x <- xs, count x xs == 2]]
