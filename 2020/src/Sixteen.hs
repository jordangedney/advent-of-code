-- | 

module Sixteen where

import Util
import qualified Data.Map as Map
import Data.List (sort)

main :: IO ()
main = do
  input <- parse . lines <$> readFile "inputs/sixteen"
  print (partOne input)
  print (partTwo input)

parse file =
  let n = splitOn ["nearby tickets:"] file !! 1 & map (splitOn ",") & map (map readInt)
      y = splitOn ["your ticket:"] file !! 1 !! 0 & splitOn "," & map readInt
      ranges = [splitOn ": " x & (!! 1)
                & splitOn " or "
                & map (splitOn "-")
                & map (\[a, b] -> (readInt a, readInt b))
                & (\[a, b] -> (a, b)) |
                x <- splitOn ["your ticket:"] file !! 0, x /= ""]
  in (ranges, y, n)

within (x, y) z = z >= x && z <= y
validRangePs rs = [\x -> within minR x || within maxR x | (minR, maxR) <- rs]

partOne (ranges, _, nearbyTickets) =
  sum [f | f <- concat nearbyTickets, map ($ f) (validRangePs ranges) & any (== True) & not]

partTwo (ranges, yourTicket, nearbyTickets) =
  let valid ticket
        = [f | f <- ticket, map ($ f) (validRangePs ranges) & any (== True)] == ticket
      validTickets = [t | t <- nearbyTickets, valid t]
      ticketFieldToRuleValidity =
        [(j, map ($ f) (validRangePs ranges)) | t <- validTickets, (j, f) <- zip [0..] t]
        & Map.fromListWith (\a b -> map (\(c, d) -> c && d) (zip a b))

      go _ [] = []
      go alreadyTaken ((_, pos, fields):xs) =
        let f = head [i | (i, v) <- zip [0..] fields, v == True, not (i `elem` alreadyTaken)]
        in (pos, f) : go (f : alreadyTaken) xs

      fieldToPos = Map.toList ticketFieldToRuleValidity
                 & map (\(a, b) -> (count True b, a, b))
                 & sort
                 & go []
                 & map (\(a, b) -> (b, a))
                 & sort

      departureValues = [yourTicket !! b | (_, b) <- take 6 fieldToPos]

  in product departureValues
