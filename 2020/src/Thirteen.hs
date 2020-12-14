-- | 

module Thirteen where

import Util

main :: IO ()
main = do
  input <- parse <$> readFile "inputs/fuck"
  input2 <- parse2 <$> readFile "inputs/fuck"
  print (partOne input)
  print (partTwo input2)

parse line =
  let [readyAt, busStartTimes] = lines line
  in (readInt readyAt, [readInt x | x <- splitOn "," busStartTimes, x /= "x"])

parse2 line =
  let [_, busStartTimes] = lines line
  in splitOn "," busStartTimes

partOne (readyAt, busStartTimes) =
  let busSchedules = map (\x -> iterate (+x) x) busStartTimes
      firstAvailable = map (dropWhile (< readyAt)) busSchedules
                       & map head
                       & (flip zip) busStartTimes
                       & minimum
  in (fst firstAvailable - readyAt) * (snd firstAvailable)

partTwo busStartTimes =
  let withOffset = [(a, readInt b) | (a, b) <- zip [0..] busStartTimes, b /= "x"]
      offsets = tail withOffset & map fst
      maxOffset = reverse withOffset & head & fst
      (b:busSchedules) =
        [iterate (+(readInt x)) (readInt x) | x <- busStartTimes, x /= "x"]
      findAlignment (toFind:xs) buses =
        let maxes = map (+toFind) offsets
            newBuses = map (\(x, y) -> dropWhile (< x) y) (zip maxes buses)
        in if maxes == (map head newBuses)
           then toFind
           else findAlignment xs newBuses
  -- in findAlignment b busSchedules & reverse & head
  in findAlignment b busSchedules


  -- partTwo busStartTimes =
--   let withOffset = [(a, readInt b) | (a, b) <- zip [0..] busStartTimes, b /= "x"]
--       withoutOffset = tail withOffset & map snd
--       offsets = tail withOffset & map fst
--       maxOffset = reverse withOffset & head & fst
--       lastBus = reverse withOffset & head & snd
--
--       withOffsetFoo = map (\(o, x) -> (o - 17, x)) withOffset
--
--       b = [blah,blah+19..]
--       -- (b:busSchedules) =
--       --   [iterate (+(readInt x)) (readInt x) | x <- busStartTimes, x /= "x"]
--       findAlignment (toFind:xs) =
--         let maxes = map (+toFind) offsets
--             buses = zip maxes withoutOffset
--
--         in if divides ((toFind + maxOffset), lastBus)
--            then if all (== True) (map divides (zip maxes withoutOffset))
--                 then toFind
--                 else findAlignment xs
--            else findAlignment xs
--   in findAlignment (dropWhile (< blah) b)

blah = 93700000000000
divides (x, y) = (x `mod` y) == 0


-- partTwo busStartTimes =
--   let withOffset = [(a, readInt b) | (a, b) <- zip [0..] busStartTimes, b /= "x"]
--       withoutOffset = tail withOffset & map snd
--       offsets = tail withOffset & map fst
--       maxOffset = reverse withOffset & head & fst
--       lastBus = reverse withOffset & head & snd
--
--       withOffsetFoo = map (\(o, x) -> (o - 17, x)) withOffset
--
--       b = [blah,blah+19..]
--       -- (b:busSchedules) =
--       --   [iterate (+(readInt x)) (readInt x) | x <- busStartTimes, x /= "x"]
--       findAlignment (toFind:xs) =
--         let maxes = map (+toFind) offsets
--             buses = zip maxes withoutOffset
--
--         in if divides ((toFind + maxOffset), lastBus)
--            then if all (== True) (map divides (zip maxes withoutOffset))
--                 then toFind
--                 else findAlignment xs
--            else findAlignment xs
--   in findAlignment (dropWhile (< blah) b)



foo = 100000000000481

