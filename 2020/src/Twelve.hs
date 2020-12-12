-- | It's always a good sign when the captain starts asking passengers for help

module Twelve where

import Util

main :: IO ()
main = do
  input <- map parseInstruction . lines <$> readFile "inputs/twelve"
  print (partOne input)
  print (partTwo input)

parseInstruction ('N':x) = N (read x)
parseInstruction ('E':x) = E (read x)
parseInstruction ('S':x) = S (read x)
parseInstruction ('W':x) = W (read x)
parseInstruction ('L':x) = L (read x)
parseInstruction ('R':x) = R (read x)
parseInstruction ('F':x) = F (read x)

data Direction = North | East | South | West deriving (Show, Eq)
data Instruction = N Int | E Int | S Int | W Int | L Int | R Int | F Int deriving (Show, Eq)
data Ship = Ship { coords :: Coords, dir :: Direction } deriving (Show, Eq)

rotateLeft North = West
rotateLeft West = South
rotateLeft South = East
rotateLeft East = North

rotateRight = rotateLeft . rotateLeft . rotateLeft

moveShip :: Ship -> Instruction -> Ship
moveShip (Ship (x, y) facing) (N amnt) = Ship (x, y + amnt) facing
moveShip (Ship (x, y) facing) (E amnt) = Ship (x + amnt, y) facing
moveShip (Ship (x, y) facing) (W amnt) = Ship (x - amnt, y) facing
moveShip (Ship (x, y) facing) (S amnt) = Ship (x, y - amnt) facing

moveShip (Ship (x, y) North)  (F amnt) = Ship (x, y + amnt) North
moveShip (Ship (x, y) East)   (F amnt) = Ship (x + amnt, y) East
moveShip (Ship (x, y) West)   (F amnt) = Ship (x - amnt, y) West
moveShip (Ship (x, y) South)  (F amnt) = Ship (x, y - amnt) South

moveShip (Ship c d)           (L 90)   = Ship c (rotateLeft d)
moveShip (Ship c d)           (L 180)  = Ship c (d & rotateLeft & rotateLeft)
moveShip (Ship c d)           (L _)    = Ship c (d & rotateLeft & rotateLeft & rotateLeft)
moveShip (Ship c d)           (R 90)   = Ship c (rotateRight d)
moveShip (Ship c d)           (R 180)  = Ship c (d & rotateRight & rotateRight)
moveShip (Ship c d)           (R _)    = Ship c (d & rotateRight & rotateRight & rotateRight)

partOne input =
  let go [] ship = [ship]
      go (x:xs) ship = ship : go xs (moveShip ship x)
  in go input (Ship (0, 0) East) & reverse & head & coords & (\(a, b) -> abs a + abs b)

type Waypoint = Coords

rotateCoordLeft :: Int -> Waypoint -> Waypoint
rotateCoordLeft 90 (x, y) = (-1 * y, x)
rotateCoordLeft 180 c = c & rotateCoordLeft 90 & rotateCoordLeft 90
rotateCoordLeft _ c = c & rotateCoordLeft 180 & rotateCoordLeft 90

rotateCoordRight 90 c = rotateCoordLeft 270 c
rotateCoordRight 180 c = c & rotateCoordRight 90 & rotateCoordRight 90
rotateCoordRight _ c = c & rotateCoordRight 180 & rotateCoordRight 90

moveShipWithWaypoint :: Ship -> Waypoint -> Instruction -> (Ship, Waypoint)
moveShipWithWaypoint ship (wx, wy) (N amnt) = (ship, (wx, wy + amnt))
moveShipWithWaypoint ship (wx, wy) (E amnt) = (ship, (wx + amnt, wy))
moveShipWithWaypoint ship (wx, wy) (W amnt) = (ship, (wx - amnt, wy))
moveShipWithWaypoint ship (wx, wy) (S amnt) = (ship, (wx, wy - amnt))

moveShipWithWaypoint ship w (L amnt)   = (ship, rotateCoordLeft amnt w)
moveShipWithWaypoint ship w (R amnt)   = (ship, rotateCoordRight amnt w)

moveShipWithWaypoint ship w (F 0) = (ship, w)
moveShipWithWaypoint (Ship (x, y) d) w@(wx, wy) (F amnt) = moveShipWithWaypoint (Ship (x + wx, y + wy) d) w (F (amnt - 1))

partTwo input =
  let go [] ship _ = [ship]
      go (x:xs) ship w = ship : (uncurry (go xs)) (moveShipWithWaypoint ship w x)
  in go input (Ship (0, 0) East) (10, 1) & reverse & head & coords & (\(a, b) -> abs a + abs b)
