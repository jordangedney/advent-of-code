module X5 (main) where

import qualified Data.Map as M

parse :: String -> ((Int, Int), (Int, Int))
parse xs =
  splitOn " -> " xs
  |> map (splitOn ",")
  |> (\[[a,b], [c,d]] -> ((read a, read b), (read c, read d)))

mkList a b | a < b = [a..b]
           | otherwise = reverse [b..a]

genLine ((a, b), (c, d)) =
  if a == c
  then if b < d then [(a, x) | x <- [b..d]]
       else reverse [(a, x) | x <- [d..b]]
  else if b == d
       then if a < c then [(x, b) | x <- [a..c]]
            else reverse [(x, b) | x <- [c..a]]
       else zip (mkList a c) (mkList b d)

part1 xs =
  let onlyStrightLines = [x | x@((a, b), (c, d)) <- xs,  (a == c) || (b == d)]
      points = map genLine onlyStrightLines |> concat
      grid :: Map (Int, Int) Int = foldr (\l r -> M.insertWith (+) l 1 r) [] points
  in M.filter (> 1) grid |> length

part2 xs =
  let points = map genLine xs |> concat
      grid :: Map (Int, Int) Int = foldr (\l r -> M.insertWith (+) l 1 r) [] points
  in M.filter (> 1) grid |> length

main :: IO ()
main = readFile "inputs/5" <&> lines >>> map parse >>> part1 >>= print
