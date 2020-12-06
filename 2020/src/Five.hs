-- | Let's see if we can't beat ghavil.

module Five where

import Util
import Data.Sort (sort)

main :: IO ()
main = do
  input <- lines <$> readFile "inputs/five"
  print (partOne input)
  print (partTwo input)

data WhichHalf = L | U

toSeatNumber ticket =
  let (row', col') = splitAt 7 ticket
      row = map (\x -> if x == 'F' then L else U) row'
      col = map (\x -> if x == 'L' then L else U) col'
      midPoint x y = (((max x y) - (min x y)) `div` 2) + (min x y)
      parse [L] minSeat _ = minSeat
      parse [U] _ maxSeat = maxSeat
      parse (L:xs) minSeat maxSeat = parse xs minSeat (midPoint minSeat maxSeat)
      parse (U:xs) minSeat maxSeat = parse xs (1 + (midPoint minSeat maxSeat)) maxSeat
  in (parse row 0 127, parse col 0 7)

seatID (row, col) = row * 8 + col

partOne input = map (seatID . toSeatNumber) input & foldr1 max

partTwo input =
  let seats = map (seatID . toSeatNumber) input & sort
  in head ([x | x <- [foldr1 min seats..foldr1 max seats], not (x `elem` seats)])
