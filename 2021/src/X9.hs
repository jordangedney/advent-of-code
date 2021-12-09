module X9 where

import Lude
import qualified Data.Map as M

pNeighbors = positivePoints . neighbors

getVals grid = catMaybes . map (flip M.lookup grid)

minima grid point =
  getVals grid (pNeighbors point)
  |> foldl1 min
  |> (\lowestNeighbor -> (grid M.! point) < lowestNeighbor)

minimaList grid = M.keys grid |> filter (minima grid)

mapFilter grid pred points = [p | p <- points, any pred (M.lookup p grid)]

basin grid minima' =
  let go seen [] = seen
      go seen toCheck =
        let lows = [p | p <- mapFilter grid (< 9) toCheck, p `notElem` seen]
        in go (seen ++ toCheck) (concat (map pNeighbors lows))
  in go [minima'] (pNeighbors minima') |> mapFilter grid (/= 9) |> nub

part1 grid = minimaList grid |> map (grid M.!) |> map (+1) |> sum

part2 grid =
  minimaList grid
  |> map (length . basin grid)
  |> take 3 . reverse . sort
  |> product

main = readFile "inputs/9" <&> lines >>> mkGrid >>> part2 >>= print
