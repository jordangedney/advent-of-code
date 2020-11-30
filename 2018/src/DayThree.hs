module DayThree where

import Util (getFile, (|>), cartesianProduct)

import Data.Char (isDigit)
import Data.List.Split (splitOn)
import qualified Data.Set as Set

claim :: String -> [Int]
claim claimStr =
  claimStr
  |> words
  |> concat . (map $ splitOn ",")
  |> concat . (map $ splitOn "x")
  |> map (filter isDigit)
  |> filter (not . null)
  |> map (\x -> (read x :: Int))

range start end = [start + 1 .. start + end]
grid [_, posX, posY, width, height] =
  Set.fromList $ cartesianProduct (range posX width) (range posY height)

intersectingClaims :: Ord a => [Set.Set a] -> Set.Set a
intersectingClaims [] = Set.empty
intersectingClaims (g:grids) =
  grids
  |> map (Set.intersection g)
  |> foldl (\l r -> Set.union l r) Set.empty
  |> Set.union (intersectingClaims grids)

partOne :: [[Int]] -> Int
partOne claims =
  claims
  |> map grid
  |> intersectingClaims
  |> length

partTwo :: [[Int]] -> [Int]
partTwo claims =
  let id_ [id, _, _, _, _] = id in
  claims
  |> map grid
  |> intersectingClaims
  |> (\intersecting ->
        claims
        |> map (\claim -> (id_ claim, grid claim))
        |> filter (\(id, grid_) -> (Set.intersection intersecting grid_) == Set.empty)
        |> map fst)

dayThreeMain :: IO ()
dayThreeMain = do
  fileData <- getFile "inputs/DayThreeInput"
  let claims = map claim $ fileData
  putStrLn $ show $ partOne claims
  putStrLn $ show $ partTwo claims
