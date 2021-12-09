module X9 where

import Lude
import qualified Data.Map as M
import Data.Sort (sortBy)
import Data.Ord (comparing)

pNeighbors = positivePoints . neighbors

getVals grid points =  map (flip M.lookup grid) points |> catMaybes

minima :: Grid -> Point -> Bool
minima grid point = getVals grid (pNeighbors point)
  |> foldl1 min
  |> (\lowestNeighbor -> (grid M.! point) < lowestNeighbor)

minimaList grid = M.keys grid |> filter (minima grid)

part1 grid = minimaList grid |> map (grid M.!) |> map (+1) |> sum

mapFilter :: Grid -> (Int -> Bool) -> [Point] -> [Point]
mapFilter grid pred [] = []
mapFilter grid pred (p:points) = case M.lookup p grid of
  Just x -> if pred x then (p : mapFilter grid pred points)
            else mapFilter grid pred points
  Nothing -> mapFilter grid pred points

basin :: Grid -> Point -> [Point]
basin grid minima' =
  let go seen [] = seen
      go seen (p:toCheck) = case M.lookup p grid of
        Just v -> if v < 9
                  then next [x | x <- (toCheck <> pNeighbors p), x `notElem` seen]
                  else next toCheck
        Nothing -> next toCheck
        where next = go (p:seen)
  in go [minima'] (pNeighbors minima') |> mapFilter grid (/= 9) |> nub

part2 grid =
  minimaList grid
  |> map (basin grid)
  |> sortBy (comparing length)
  |> reverse
  |> take 3
  |> map length
  |> product

main :: IO ()
main = readFile "inputs/9" <&> lines >>> mkGrid >>> part2 >>= print
