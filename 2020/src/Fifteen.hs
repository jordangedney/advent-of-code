-- | 

module Fifteen where

import Util
import qualified Data.Map as Map

main :: IO ()
main = do
  let input = [8,13,1,0,18,9]
  print (partOne input)
  print (partTwo input)

countingGame toFind' input =
  let alreadyCalled = zip input [1..] & map (\(a, b) -> (a, (b, (-1)))) & Map.fromList
      go _ _ lastSpoken 0 = lastSpoken
      go seen currentTurn lastSpoken turnsLeft =
        let newNumber = case Map.findWithDefault (lastSpoken, -1) lastSpoken seen of
                          (_, -1) -> 0
                          (x, y) -> x - y
            insertExisting (a, _) (b, c) = (a, b)
            withNewNumber =
              Map.insertWith insertExisting newNumber (currentTurn, -1) seen
        in go withNewNumber (currentTurn + 1) newNumber (turnsLeft - 1)
      toFind = toFind' - length input
  in go alreadyCalled (length input + 1) (head (reverse input)) toFind

partOne = countingGame 2020
partTwo = countingGame 30000000
