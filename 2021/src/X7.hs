module X7 (main) where

import Lude

parse xs = splitOn "," xs |> map read

solve :: (Int -> Int -> Int) -> [Int] -> Int
solve costToCrab crabs = map fuelCost crabs |> foldl1 min
  where fuelCost pos = range 0 (maximum crabs)
                     |> map (\i -> crabs `count` i * costToCrab pos i)
                     |> sum

delta a b = max a b - min a b

part1 = solve delta
part2 = solve (\pos p -> sum (range 0 (delta pos p)))

main :: IO ()
main = readFile "inputs/7" <&> lines >>> concat >>> parse >>> part2 >>= print
