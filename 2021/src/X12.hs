module X12 (main) where

import Lude
import qualified Data.Map as M
import Data.Char (isUpper)

parse xs =
  map (splitOn "-") xs
  |> (\ys -> ys ++ (map reverse ys))
  |> foldl (\m [from, to] -> M.insertWith (++) from [to] m) M.empty
  |> M.map (sort . nub)

pathDone :: [String] -> Bool
pathDone = (== "end") . head . reverse

part1 cave = doFixed hack [["start"]] |> last |> length
  where go [] = [[]]
        go (p:paths) =
          if pathDone p then [p] ++ go paths
          else let n = last p
                   newPaths = [p ++ [x] | x <- toVisit n p]
               in newPaths ++ go paths
        toVisit n path = filter (visitable path) $ cave M.! n
        visitable path y@(x:_) = isUpper x || y `notElem` path

        hack ps = filter (not . null) (go ps)

hasDoubles [] = False
hasDoubles (p:ps) = p `elem` ps || hasDoubles ps

justSmall = filter (not . isUpper . head)

canVisit nodes y@(x:_) = isUpper x ||
                   if (hasDoubles . justSmall $ nodes) then y `notElem` nodes
                   else True

part2 cave = doFixed hack [["start"]] |> last |> length
  where go [] = [[]]
        go (p:paths) =
          if pathDone p then [p] ++ go paths
          else let n = last p
                   newPaths = [p ++ [x] | x <- toVisit n p, x /= "start"]
               in newPaths ++ go paths
        toVisit n path = filter (canVisit path) $ cave M.! n
        hack ps = filter (not . null) (go ps)

main :: IO ()
main = readFile "inputs/12" <&> lines >>> parse >>> part2 >>= print
