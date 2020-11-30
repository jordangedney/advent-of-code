module DaySix where

import Util (getFile, (|>), count)
import Data.List.Split (splitOn)
import Data.List (transpose, minimumBy, nub, maximumBy)
import qualified Data.Map as Map

testData :: [(Int, Int)]
testData =
  [(1, 1)
  ,(1, 6)
  ,(8, 3)
  ,(3, 4)
  ,(5, 5)
  ,(8, 9)
  ]

toPoints xs = [((read x :: Int), (read y :: Int)) | [x, y] <- map (splitOn ", ") xs]
manhattanDistance (x1, y1) (x2, y2) =  abs (y2 - y1) + abs (x2 - x1)
yPos = snd
xPos = fst

findPoint :: ((Int, Int) -> Int)
          -> (Int -> Int -> Bool)
          -> [(Int, Int)]
          -> (Int,Int)
findPoint pos cmp = foldl1 (\p1 p2 -> if (cmp (pos p1) (pos p2)) then p1 else p2)

top = findPoint yPos (<=)
bottom = findPoint yPos (>=)
left = findPoint xPos (<=)
right = findPoint xPos (>=)
boundary points = [top points, bottom points, left points, right points]

grid :: [(Int, Int)] -> [(Int, Int)]
grid points = [(x, y) | y <- [yPos top..yPos bottom], x <- [xPos left..xPos right]]
   where [top, bottom, left, right] = boundary points

-- withoutBoundary :: [(Int, Int)] -> [(Int, Int)]
-- withoutBoundary points =
--   points |> filter (\p -> not $ or [(yPos p) == (yPos $ top points),
--                                     (yPos p) == (yPos $ bottom points),
--                                     (xPos p) == (xPos $ left points),
--                                     (xPos p) == (xPos $ right points)])

alongBoundary :: [(Int, Int)] -> (Int, Int) -> Bool
alongBoundary points p =
  or [(yPos p) == (yPos $ top points),
       (yPos p) == (yPos $ bottom points),
       (xPos p) == (xPos $ left points),
       (xPos p) == (xPos $ right points)]

fst' (x, _, _) = x
thrd (_, _, x) = x


manhattanDistances distanceToPoints startingPoints =
  startingPoints
  |> map (\p -> zip (repeat p) (map (\x -> (x, (manhattanDistance p x))) distanceToPoints))
  |> map (map (\(p, (p1, d)) -> (p, p1, d)))

partOne points =
  let hasMultipleClosestPoints xs = (count (foldl1 min distances) distances) > 1
         where distances = map thrd xs
  in
  points
  |> manhattanDistances (grid points)
  |> transpose
  |> filter (not . hasMultipleClosestPoints)
  |> map (minimumBy (\x y -> compare (thrd x) (thrd y)))
  -- |> filter (\(point, _, _) -> (not $ alongBoundary points point))
  |> filter (not . (alongBoundary points) . fst')
  |> map fst'
  |> (\numberOfClosest -> [(x, count x numberOfClosest) | x <- nub numberOfClosest])
  |> maximumBy (\x y -> compare (snd x) (snd y))

partTwo points =
  grid points
  |> manhattanDistances points
  |> filter ((<10000) . sum . map thrd)
  |> length

daySixMain :: IO ()
daySixMain = do
  fileData <- getFile "inputs/DaySixInput" >>= pure . toPoints
  putStrLn $ show $ partOne fileData
  putStrLn $ show $ partTwo fileData
  pure ()
