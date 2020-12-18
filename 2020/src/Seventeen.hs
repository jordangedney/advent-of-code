-- | -----------------------------------------------------------------------------------------------
-- | Simulating life, one keystroke at a time.
-- | -----------------------------------------------------------------------------------------------

module Seventeen where

import Util
import qualified Data.Map as Map
import Data.List.Split (chunksOf)

main :: IO ()
main = do
  input <- parse <$> readFile "inputs/seventeen"
  print (partOne input)

data State = Active | Inactive deriving (Show, Eq, Ord)
type Coord3D = (Int, Int, Int)

parseState '#' = Active
parseState _ = Inactive

parse file
  = zip [0..] (lines file)
  & map (\(a, b) -> (a, (zip [0..] b)))
  & map (\(a, b) -> map (\(c, d) -> ((a, c, 0), parseState d)) b)
  & concat
  & Map.fromList

neighbors (x, y, z)
  = [ (a, b, c)
    | a <- [x-1..x+1], b <- [y-1..y+1], c <- [z-1..z+11]
    , (a, b, c) /= (x, y, z)]

partOne input =
  let go game 0 = game
      go game cyclesLeft =
        let newState' (pos, Active) =
              if lenActiveNeighbors pos == 2 || lenActiveNeighbors pos == 3
              then Active
              else Inactive
            newState' (pos, Inactive) =
              if lenActiveNeighbors pos == 3
              then Active
              else Inactive

            newState pos =  newState' (pos, (get pos))

            get coord = Map.findWithDefault Inactive coord game

            lenActiveNeighbors coord
              = neighbors coord
              & filter (\c -> get c == Active)
              & length

            fst' (a, _, _) = a
            snd' (_, b, _) = b
            thd' (_, _, c) = c
            getAxis axisGetter = Map.keys game & map axisGetter
            [minX, minY, minZ] = map minimum [getAxis fst', getAxis snd', getAxis thd']
            [maxX, maxY, maxZ] = map maximum [getAxis fst', getAxis snd', getAxis thd']

            coordsToCheck = [(x, y, z) | x <- [minX-1..maxX+1]
                                       , y <- [minY-1..maxY+1]
                                       , z <- [minZ-1..maxZ+1]]
            newGame = Map.fromList (map (\c -> (c, newState c)) coordsToCheck)

         in go newGame (cyclesLeft - 1)
  in go input 6 & Map.toList & map snd & count Active

prettyPrint game = do
  let gridSize = 2
      z = 0
      sliceSize = [(a, b, z) | a <- [0..gridSize], b <- [0..gridSize]]
      slice = map (\k -> Map.findWithDefault Inactive k game) sliceSize
      parseState' Active = '#'
      parseState' Inactive = '.'
      asString = map (map parseState') (chunksOf (gridSize + 1) slice)
  mapM_ print asString
