-- | Here we go.

module One (main, partOne, partTwo) where

partOne input =
  let (a, b):_ = [(x, y) | x <- input, y <- input, (x + y) == 2020]
  in a * b

partTwo input =
  let (a, b, c):_ = [(x, y, z) | x <- input, y <- input, z <- input, (x + y + z) == 2020]
  in a * b * c

main :: IO ()
main = do
  input <- map read . lines <$> readFile "inputs/one"
  print (partOne input)
  print (partTwo input)
