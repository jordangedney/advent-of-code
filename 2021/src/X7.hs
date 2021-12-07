module X7 (main) where

import Lude

parse xs = splitOn "," xs |> map read

solve :: (Int -> Int -> Int) -> [Int] -> (Int, Int)
solve costFn xs =
  let counts = frequencies xs
      getCount g = findWithDefault 0 g counts
      possiblePos = [0..foldl1 max xs]
      costs pos = [costFn pos p | p <- possiblePos]
                  |> zip [0..]
                  |> map (\(i, v) -> getCount i * v)
                  |> sum
  in map costs possiblePos
     |> zip [0..]
     |> foldl1 (\(a, b) (c, d) -> if b < d then (a, b) else (c, d))

delta a b = max a b - min a b

part1 = solve delta
part2 = solve (\pos p -> sum (range 0 (delta pos p)))

main :: IO ()
main = readFile "inputs/7" <&> lines >>> concat >>> parse >>> part2 >>= print
