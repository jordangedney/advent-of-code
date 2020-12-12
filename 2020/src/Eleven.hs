{-# LANGUAGE ScopedTypeVariables #-}

-- | I love the idea of people getting off the ferry because they have to sit next to someone.

module Eleven where

import Util
import qualified Data.Map.Strict as Map
import Data.List.Split (chunksOf)

main :: IO ()
main = do
  input <- parseSeats <$> readFile "inputs/eleven"
  file <- lines <$> readFile "inputs/eleven"
  let (xAxisLen, yAxisLen) = ((length (file !! 0)), length file)

  -- mapM_ (\(a, b) -> do
  --           mapM_ print (printSeats 10 a)
  --           print b)
  --           (partTwo input xAxisLen yAxisLen)

  print (partOne input)
  print (partTwo input xAxisLen yAxisLen)

printSeats length seats =
  let undo Person = '#'
      undo Seat = 'L'
      undo Floor = '.'
  in chunksOf length (Map.toList seats & map snd & map undo)

data Spot = Person | Seat | Floor deriving (Show, Eq)

index :: [[a]] -> [((Int, Int), a)]
index lol = zip [0..] lol & map (\(i, l) -> zip [0..] l & map (\(j, x) -> ((i, j), x))) & concat

parseSeats seats =
  let parsed = lines seats & map (map (\x -> if x == 'L' then Seat else Floor))
  in Map.fromList (index parsed)

adjacents (a, b) = [(x, y) | x <- [a-1..a+1], y <- [b-1..b+1], (a, b) /= (x, y)]

chessStyleCoords :: (Int, Int) -> Int -> Int -> [[(Int, Int)]]
chessStyleCoords coord xAxisLen yAxisLen =
  -- Don't worry about the x axis being out of bounds because things should default to floor
  let upperLeft (a, b) = [(a - x,  b - x) | x <- [1..max a b], ((a - x) >= 0)]
      upper (a, b) = [(a - x,  b) | x <- [1..max a b], ((a - x) >= 0)]
      upperRight (a, b) = [(a - x,  b + x) | x <- [1..max a b], ((a - x) >= 0)]

      -- Likewise, but ignore y axis
      left (a, b) = [(a,  b - x) | x <- [1..b]]
      right (a, b) = [(a,  b + x) | x <- [1..xAxisLen]]

      -- Yeah, I'm lazy; Overgenerate
      lowerLeft (a, b) = [(a + x,  b - x) | x <- [1..yAxisLen]]
      lower (a, b) = [(a + x,  b) | x <- [1..yAxisLen]]
      lowerRight (a, b) = [(a + x,  b + x) | x <- [1..max xAxisLen yAxisLen]]

      directions = [upperLeft, upper, upperRight, left, right, lowerLeft, lower, lowerRight]
  in map ($ coord) directions

visibleSeats :: (Int, Int) -> Map.Map (Int, Int) Spot -> Int -> Int -> [(Int, Int)]
visibleSeats coord seats xAxisLen yAxisLen =
  let lineOfSight = chessStyleCoords coord xAxisLen yAxisLen
      (avail :: [[((Int, Int),  Spot)]]) =
        lineOfSight & map (map (\s -> (s, Map.findWithDefault Floor s seats)))
      (onlyChairs :: [[(Int, Int)]]) = avail & map (\x -> [a | (a, b) <- x, b /= Floor])
      hack = onlyChairs & map (\x -> if x == [] then coord else head x)
  in [x | x <- hack, x /= coord]

partTwo input xAxisLen yAxisLen =
  let go seats =
        let getSeat s = Map.findWithDefault Floor s seats
            changedSeats = [(s, newStatus s) | s <- Map.keys seats, getSeat s /= newStatus s]
            newStatus seat =
              if getSeat seat == Floor then Floor
              else visibleSeats seat seats xAxisLen yAxisLen
                   & map getSeat
                   & count Person
                   & (\c -> if c >= 5 then Seat else if c == 0 then Person else (getSeat seat))
            newSeats = foldr (\(c, s) m -> Map.insert c s m) seats changedSeats
        in if changedSeats == [] then [(seats, changedSeats)] else [(seats, changedSeats)] ++ go newSeats
  -- in take 3 $ go input
  in go input & reverse & head & fst & Map.elems & count Person

partOne input =
  let go seats =
        let getSeat s = Map.findWithDefault Floor s seats
            changedSeats = [(s, newStatus s) | s <- Map.keys seats, getSeat s /= newStatus s]
            newStatus seat =
              if getSeat seat == Floor then Floor
              else adjacents seat
                   & map getSeat
                   & count Person
                   & (\c -> if c >= 4 then Seat else if c == 0 then Person else (getSeat seat))
            newSeats = foldr (\(c, s) m -> Map.insert c s m) seats changedSeats
        in if changedSeats == [] then [(seats, changedSeats)] else [(seats, changedSeats)] ++ go newSeats
  in go input & reverse & head & fst & Map.elems & count Person
