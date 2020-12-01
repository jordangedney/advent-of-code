-- | Here we go.

module One (main, partOne, partTwo) where

partOne input =
  let (a, b):_ = [(x, y) | x <- input, y <- input, (x + y) == 2020]
  in a * b

partTwo = undefined

main :: IO ()
main = do
  input <- map read . lines <$> readFile "inputs/one"
  print (partOne input)
