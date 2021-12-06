module X1 (main) where

import Lude

partOne :: [Int] -> Int
partOne xs = length [b | (a, b) <- zip xs (tail xs), b > a]

partTwo :: [Int] -> Int
partTwo xs = partOne [a + b + c | (a, b, c) <- zip3 xs (tail xs) (tail (tail xs))]

main :: IO ()
main = do
  input <- map read . lines <$> readFile "inputs/1"
  print (partOne input)
  print (partTwo input)
