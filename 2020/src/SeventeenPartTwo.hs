-- | -----------------------------------------------------------------------------------------------
-- | Simulating life, one keystroke, twice.
-- | -----------------------------------------------------------------------------------------------

module SeventeenPartTwo where

import Util
import qualified Data.Map as Map

main :: IO ()
main = do
  input <- parse <$> readFile "inputs/seventeen"
  print (partTwo input)

data State = Active | Inactive deriving (Show, Eq, Ord)

parseState '#' = Active
parseState _ = Inactive

parse file
  = zip [0..] (lines file)
  & map (\(a, b) -> (a, (zip [0..] b)))
  & map (\(a, b) -> map (\(c, d) -> ((a, c, 0, 0), parseState d)) b)
  & concat
  & Map.fromList

neighbors (x, y, z, w)
  = [ (a, b, c, d)
    | a <- [x-1..x+1], b <- [y-1..y+1], c <- [z-1..z+1], d <- [w-1..w+1]
    , (a, b, c, d) /= (x, y, z, w)]

partTwo input =
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

            fst' (a, _, _, _) = a
            snd' (_, b, _, _) = b
            thd' (_, _, c, _) = c
            frh' (_, _, _, d) = d
            getAxis axisGetter = Map.keys game & map axisGetter
            [minX, minY, minZ, minW]
              = map minimum [getAxis fst', getAxis snd', getAxis thd', getAxis frh']
            [maxX, maxY, maxZ, maxW]
              = map maximum [getAxis fst', getAxis snd', getAxis thd', getAxis frh']

            coordsToCheck = [(x, y, z, w) | x <- [minX-1..maxX+1]
                                          , y <- [minY-1..maxY+1]
                                          , z <- [minZ-1..maxZ+1]
                                          , w <- [minW-1..maxW+1]]
            newGame = Map.fromList (map (\c -> (c, newState c)) coordsToCheck)

         in go newGame (cyclesLeft - 1)
  in go input 6 & Map.toList & map snd & count Active
