-- | This is what happens when Apple drives the market.

module Ten where

import Util
import Data.List (sort)
import qualified Data.Map.Strict as Map

main :: IO ()
main = do
  input <- map read . lines <$> readFile "inputs/ten"
  print (partOne input)
  print (partTwo input)

partOne input =
  let input' = 0 : sort input
      differences = zip (tail input') input' & map (uncurry (-))
  in (count 1 differences) * (count 3 differences + 1)

recursiveTail [] = [[]]
recursiveTail lst@(_:xs) = lst : recursiveTail xs

validAdapter [] = (0, [])
validAdapter (x:xs) = (x,  [y | y <- (take 3 xs), y - x <= 3])
validAdapters xs = map validAdapter xs & init & init

partTwo input' =
  let input = 0 : sort input' ++ [maximum input' + 3]
      nodeToNext = Map.fromList [x | x <- recursiveTail input & validAdapters]
      nodeCosts =
        let (_:inpt) = reverse input
            go [] costs = costs
            go (x:toFind) costs =
              let nodes = Map.findWithDefault [] x nodeToNext
                  cost = sum [Map.findWithDefault 1 y costs | y <- nodes]
                  newMap = Map.insert x cost costs
              in go toFind newMap
        in go inpt Map.empty
  in Map.findWithDefault 0 0 nodeCosts
