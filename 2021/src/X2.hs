module X2 (main) where

data Movement = Forward Int | Down Int | Up Int deriving (Eq, Show)

parse :: String -> Movement
parse xs = read (words xs !! 1) |> case xs of
  ('f':_) -> Forward
  ('d':_) -> Down
  ('u':_) -> Up

part1 :: [Movement] -> Int
part1 xs = foldl go (0, 0) xs |> (\(a, b) -> a * b)
  where go (depth, horizontal) = \case
          Forward x -> (depth, horizontal + x)
          Down    x -> (depth + x, horizontal)
          Up      x -> (depth - x, horizontal)

part2 :: [Movement] -> Int
part2 xs = foldl go (0, 0, 0) xs |> (\(a, b, _) -> a * b)
  where go (depth, horizontal, aim) = \case
          Forward x -> (depth + (aim * x), horizontal + x, aim)
          Down    x -> (depth, horizontal, aim + x)
          Up      x -> (depth, horizontal, aim - x)

main :: IO ()
main = readFile "inputs/2" <&> lines >>> map parse >>> part2 >>= print
