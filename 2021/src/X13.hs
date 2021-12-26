module X13 (main) where

import Lude
-- import qualified Data.Map as M
import Control.Monad (forM, forM_)

data Fold = X Int | Y Int deriving (Show, Eq)

parse xs = splitOn [""] xs
 |> (\[pnts, folds] -> (map parsePoint pnts, map parseFold folds))
 where parsePoint p = splitOn "," p |> map readInt |> (\[x, y] -> (x, y))
       parseFold f = last (words f) |> splitOn "=" |> (\[axis, amt] ->
                                                         case axis of
                                                           "y" -> Y (readInt amt)
                                                           _   -> X (readInt amt))

fold i j = if i > j then i - ((i - j) * 2) else i

doFold (Y amt) (x, y) = (x, fold y amt)
doFold (X amt) (x, y) = (fold x amt, y)

part1 (points, folds) = map (doFold (head folds)) points |> nub |> length

part2 (points, folds) = map doFold folds
  |> foldl (\ptns f -> nub (map f ptns)) points
  |> showMap
  |> unlines

showMap :: [Point] -> [String]
showMap points =
  let maxX = foldl1 max (map fst points)
      maxY = foldl1 max (map snd points)
  in [[if (x, y) `elem` points then '#' else '.' | x <- [0..maxX]] | y <- [0..maxY]]

main :: IO ()
main = readFile "inputs/13" <&> lines >>> parse >>> part2 >>= putStrLn
