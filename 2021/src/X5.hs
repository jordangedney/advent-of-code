module X5 (main) where

import qualified Data.Map as M

parse :: String -> ((Int, Int), (Int, Int))
parse xs =
  splitOn " -> " xs
  |> map (splitOn ",")
  |> (\[[a,b], [c,d]] -> ((read a, read b), (read c, read d)))

mkLine :: (Point, Point) -> [Point]
mkLine (e@(a, b), f@(c, d))
  | e == f = [e]
  | otherwise = e : mkLine ((step a c, step b d), f)
  where step x y | x == y = x
                 | x < y = x + 1
                 | x > y = x - 1

part1 xs = part2 [x | x@((a, b), (c, d)) <- xs,  (a == c) || (b == d)]
part2 xs = map mkLine xs |> concat |> frequencies |> M.filter (> 1) |> length

main :: IO ()
main = readFile "inputs/5" <&> lines >>> map parse >>> part2 >>= print
