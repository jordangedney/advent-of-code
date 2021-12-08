module X8 where

import Lude

parse xs = splitOn "|" xs |> map words

knownSizes :: [Int]
knownSizes = [2, 3, 4, 7]

part1 xs = map (!! 1) xs
  |> (\ys -> [x | y <- ys, x <- y,  length x `elem` knownSizes])
  |> length

a `has` b = all (`elem` a) b
a `no` b = not (a `has` b)

rules xs =
  let one    = head [x | x <- xs, length x == 2]
      seven  = head [x | x <- xs, length x == 3]
      four   = head [x | x <- xs, length x == 4]
      nine   = head [x | x <- xs, length x == 6, x `has` one, x `has` four]
      zero   = head [x | x <- xs, length x == 6, x `has` one, x `no` four]
      six    = head [x | x <- xs, length x == 6, x `no` one,  x `no` four]
      three  = head [x | x <- xs, length x == 5, x `has` one]
      five   = head [x | x <- xs, length x == 5, nine `has` x, x `no` three]
      two    = head [x | x <- xs, length x == 5, nine `no` x]
      eight  = head [x | x <- xs, length x == 7]
  in [zero, one, two, three, four, five, six, seven, eight, nine]

getRules [i, o] =
  (map sort i <> map sort o)
  |> nub
  |> rules
  |> zip [0..]

translate rlz num = filter (\(a, b) -> b == sort num) rlz |> head |> fst

go x@[_, o]= map (translate (getRules x)) o

fromDigits = foldl (\n d -> 10*n + d) 0

part2 xs = map (fromDigits . go) xs |> sum

main :: IO ()
main = readFile "inputs/8" <&> lines >>> map parse >>> part2 >>= print
