module Eight where

import Util (count, digits)
import Data.List.Split (chunksOf)
import Data.List (transpose)

progMain :: IO ()
progMain = do
  input <- chunksOf (25 * 6) . digits . read <$> readFile "inputs/eight"

  print $ (\(_, a, b) -> a * b) $ minimum [(count 0 x, count 1 x, count 2 x) | x <- input]

  mapM_ putStrLn $ chunksOf 25 $
    map ((\a -> if a == 1 then '*' else ' ') . head . filter (/= 2)) $ transpose input
