module X7 (main) where

import Lude

parse xs = splitOn "," xs |> map read

xs `count` x = length (filter (== x) xs)

solve :: (Int -> Int -> Int) -> [Int] -> (Int, Int)
solve costToCrab crabs =
  let possibleCrabs = [0..foldl1 max crabs]

      fuelCost position =
        sum [crabs `count` i * costToCrab position i | i <- possibleCrabs]

      findMin x@(minPos, minFuel) newPos =
        if fuelCost newPos < minFuel then (newPos, fuelCost newPos) else x

  in foldl findMin (-1, 999999999999999999) crabs

delta a b = max a b - min a b

part1 = solve delta
part2 = solve (\pos p -> sum (range 0 (delta pos p)))

main :: IO ()
main = readFile "inputs/7" <&> lines >>> concat >>> parse >>> part1 >>= print
