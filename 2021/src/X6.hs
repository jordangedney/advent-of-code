module X6 (main) where

import Lude

parse xs =
  splitOn "," xs
  |> map read
  |> frequencies
  |> (\freqs -> (\idx -> findWithDefault 0 idx freqs))
  |> (\get -> (get 0, get 1, get 2, get 3, get 4, get 5, get 6, get 7, get 8))

go (a, b, c, d, e, f, g, h, i) =
   (b, c, d, e, f, g, a + h, i, a)

countFish (a, b, c, d, e, f, g, h, i) = a + b + c + d + e + f + g + h + i

part1 xs = countFish (iterate go xs !! 80)
part2 xs = countFish (iterate go xs !! 256)

main :: IO ()
main = readFile "inputs/6" <&> lines >>> concat >>> parse >>> part2 >>= print
