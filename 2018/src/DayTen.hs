module DayTen where

import Util ((|>), toInt, apply, getFile)
import Data.List.Split (splitOn)
import Data.List (minimumBy, maximumBy)
import Control.Concurrent (threadDelay)

parser pointStr =
  splitOn "<" pointStr
  |>  (\x -> [x !! 1, x !! 2])
  |> concat . map (map toInt . (splitOn ",") . head . splitOn ">")

data Boundary = Boundary Int Int Int Int deriving (Show, Eq)

left (Boundary l _ _ _) = l
up (Boundary _ u _ _) = u
right (Boundary _ _ r _) = r
down (Boundary _ _ _ d) = d

mkBoundary points = Boundary left up right down
  where left = fst' $ minimumBy (comparing fst') points
        up = snd' $ minimumBy (comparing snd') points
        right = fst' $ maximumBy (comparing fst') points
        down = snd' $ maximumBy (comparing snd') points
        (fst', snd') = (head, head . tail)
        comparing getter x y = compare (getter x) (getter y)

type Points = [[Int]]
data Grid = Grid Points Boundary deriving (Show, Eq)

alignPointsWithBoundary (Grid points b@(Boundary l u _ _)) =
  Grid (map (\(x:y:velocity) -> (x - l):(y - u):velocity) points) b

mkGrid points = alignPointsWithBoundary $ Grid points (mkBoundary points)

blankGraph (Grid _ (Boundary l u r d)) =
  take (d - u + 1) $ repeat $ take (r - l + 1) $ repeat "."

replace list index element = take index list ++ [element] ++ drop (index + 1) list
replaceGrid grid xIndex yIndex element = replace grid yIndex newList
  where newList = replace (grid !! yIndex) xIndex element

generateGraph g@(Grid points _) = foldr go (blankGraph g) points
  where go (xPos:yPos:_) graph = replaceGrid graph xPos yPos "#"

updatePoints (Grid points b) =
  Grid (map (\[xPos, yPos, vX, vY] -> [xPos + vX, yPos + vY, vX, vY]) points) b

printGrid grid = mapM_ putStrLn $ map unwords grid

showMessage grid = do
  let updated = updatePoints grid
  printGrid . generateGraph $ updated
  threadDelay 1000000
  showMessage updated

-- The input size is huge- update the grid until the points will fit on the screen:
shrinkGrid time g@(Grid points (Boundary l u r d)) =
  if or [r - l > 70, u - d > 70]
  then shrinkGrid (time + 1) $ (\(Grid p b) -> mkGrid p) (updatePoints g)
  else (g, time)

dayTenMain = do
  let fileData = (424, 71144)
  fileData <- getFile "inputs/DayTenInput" >>= pure . (mkGrid . (map parser))
  -- showMessage fileData
  pure ()
