module X6 (main) where

import Lude

data Fish = Fish Int Int Int Int Int Int Int Int Int deriving Show

parse :: String -> Fish
parse xs = xs |> (splitOn ",") |> map read |> frequencies
  |> (\f -> (\x -> findWithDefault 0 x f))
  |> (\g -> Fish (g 0) (g 1) (g 2) (g 3) (g 4) (g 5) (g 6) (g 7) (g 8))

go :: Fish -> Fish
go (Fish x1 x2 x3 x4 x5 x6 x7 x8 x9) = (Fish x2 x3 x4 x5 x6 x7 (x8 + x1) x9 x1)

count :: Fish -> Int
count (Fish x1 x2 x3 x4 x5 x6 x7 x8 x9) = x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9

part1 xs = iterate go xs |> take 81 |> last |> count

part2 xs = iterate go xs |> take 257 |> last |> count

main :: IO ()
main = readFile "inputs/6" <&> lines >>> concat >>> parse >>> part2 >>= print
