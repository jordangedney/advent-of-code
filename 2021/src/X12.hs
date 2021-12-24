module X12 (main) where

import Lude
import Data.Char (isUpper)

parse xs =
  map (splitOn "-") xs
  |> (\ys -> ys ++ (map reverse ys))
  |> foldl (\m [from, to] -> M.insertWith (++) from [to] m) M.empty
  |> M.map (sort . nub)

search visitable except cave = doFixed hack [["start"]] |> last |> length
  where go [] = [[]]
        go (p:paths) =
          if pathDone p then [p] ++ go paths
          else let n = last p
                   newPaths = [p ++ [x] | x <- toVisit n p, except x]
               in newPaths ++ go paths
        toVisit n path = filter (visitable path) $ cave M.! n
        hack ps = filter (not . null) (go ps)
        pathDone = (== "end") . head . reverse

part1 = search v (const True)
  where v path y@(x:_) = isUpper x || y `notElem` path

part2 cave = search canVisit (/= "start")
  where hasDoubles [] = False
        hasDoubles (p:ps) = p `elem` ps || hasDoubles ps

        justSmall = filter (not . isUpper . head)

        canVisit nodes y@(x:_) =
          isUpper x || if (hasDoubles . justSmall $ nodes) then y `notElem` nodes
                       else True

main :: IO ()
main = readFile "inputs/12" <&> lines >>> parse >>> part2 >>= print
