module DayEleven where

import Util ((|>), cartesianProduct)
import Data.List (sortBy)
import Data.Ord (comparing)
import qualified Data.Map as Map

-- serialNumber = 4151
-- gridSize = 300
serialNumber = 18
gridSize = 300
grid = cartesianProduct [1..gridSize - 1] [1..gridSize - 1]

powerLevelMap = map (\c -> (c, powerLevel' c)) grid |> Map.fromList

powerLevel' (xPos, yPos) =
  xPos + 10
  |> (\rackID ->
        rackID
        |> (* yPos)
        |> (+ serialNumber)
        |> (* rackID)
        |> (`mod` 10) . (`div` 100)
        |> (+ (-5)))

powerLevel coord =
  case Map.lookup coord powerLevelMap of
    Just v -> v
    Nothing -> 0

nextLayer (xPos, yPos) amount =
  [(x, yPos + amount) | x <- [xPos..xPos + amount]] ++
  [(xPos + amount, y) | y <- [yPos..yPos + amount - 1]]

powerLevels coords = scanl go (blockLevel [coords]) [1..]
  where go prevPowerLevel squareSize = prevPowerLevel + blockLevel (next squareSize)
        next = nextLayer coords
        blockLevel adjacent = sum $ map powerLevel adjacent

validCoords squareSize max = cartesianProduct [1..max - squareSize] [1..max - squareSize]

temp size = foldr1 go $ validCoords size gridSize
  where go coord maxCoord =
          if (powerLevels coord !! size) > powerLevels maxCoord !! size
          then coord else maxCoord

partOne = temp 2

partTwo = foldr go ((0, 0), 0, 0) grid
  where
    go coord@(xPos, yPos) m@(_, prevPLevel, _) =
          let maxSquareSize = min (gridSize - xPos) (gridSize - yPos)
              maxPowerLevel = foldr1 max $ take maxSquareSize $ powerLevels coord
          in
            if maxPowerLevel > prevPLevel then (coord, maxPowerLevel, 0) else m

dayElevenMain = do
  putStrLn $ show $ partOne
  putStrLn $ show $ partTwo
