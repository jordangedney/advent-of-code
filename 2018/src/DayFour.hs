module DayFour where

import Util (getFile, (|>), count)
import Data.Char (isDigit)
import Data.List.Split (splitOn, split, whenElt, chunksOf)
import qualified Data.Map as Map
import Data.List (isInfixOf, sort, group)

-- Returns a really terrible format where single ids are denoted as a list of
-- one element, and times are lists of even elements
guards :: [String] -> [[Int]]
guards data_ =
  let isGuard xs = "Guard" `isInfixOf` xs
      guardId x = (read (splitOn "#" (words x !! 3) !! 1) :: Int)
      seconds x = (read ((splitOn ":" ((splitOn "]" (words x !! 1)) !! 0)) !! 1) :: Int)
  in
  data_
  |> split (whenElt isGuard)
  |> filter (not . null)
  |> map (map (\x ->
                if isGuard x then guardId x
                else seconds x))

-- I had a clever
filterGuardsWhichArentSleeping guardData = reverse $ lol $ reverse guardData
  where lol (maybeSleepingTime:guardId:xs) =
          if isSleepingTime maybeSleepingTime
          then [(guardId, maybeSleepingTime)] ++ lol xs
          else lol (guardId:xs)
        lol _ = []
        isSleepingTime lst = (length lst) > 1
unpackShittyGuardFormat ([x], y) = (x, y)

calculateSleepingTime (guardId, sleepingTimes) =
  sleepingTimes
  |> chunksOf 2
  |> map (\[fellAsleep, wokeUp] -> [fellAsleep..wokeUp-1])
  |> concat
  |> (\minutesAsleep -> (guardId, minutesAsleep))

mostCommon :: Ord a => [a] -> a
mostCommon = snd . maximum . map (\xs -> (length xs, head xs)) . group . sort

partOne guardData =
  guardData
  |> sort
  |> guards
  |> filterGuardsWhichArentSleeping
  |> map unpackShittyGuardFormat
  |> map calculateSleepingTime
  -- Are you fucking kidding me Haskell? Dictionary inputs have to be sorted
  -- wtf!
  |> sort
  |> Map.fromAscListWith (++)
  |> Map.toList
  |> longestSleeper
  |> (\(guardId, sleepingRange) -> (guardId, mostCommon sleepingRange))
  |> (\(guardId, mostCommonMinuteSlept) -> guardId * mostCommonMinuteSlept)
  where longestSleeper (x:xs) = maxLen x xs
          where maxLen currentMax [] = currentMax
                maxLen (guardId, sleepingTimes) ((nextGuard, nextSleeping):ns)
                  | length sleepingTimes < length nextSleeping =
                      maxLen (nextGuard, nextSleeping) ns
                  | otherwise = maxLen (guardId, sleepingTimes) ns
x =
  [ "[1518-11-01 00:00] Guard #10 begins shift"
  , "[1518-11-01 00:05] falls asleep"
  , "[1518-11-01 00:25] wakes up"
  , "[1518-11-01 00:30] falls asleep"
  , "[1518-11-01 00:55] wakes up"
  , "[1518-11-01 23:58] Guard #99 begins shift"
  , "[1518-11-02 00:40] falls asleep"
  , "[1518-11-02 00:50] wakes up"
  , "[1518-11-03 00:05] Guard #10 begins shift"
  , "[1518-11-03 00:24] falls asleep"
  , "[1518-11-03 00:29] wakes up"
  , "[1518-11-04 00:02] Guard #99 begins shift"
  , "[1518-11-04 00:36] falls asleep"
  , "[1518-11-04 00:46] wakes up"
  , "[1518-11-05 00:03] Guard #99 begins shift"
  , "[1518-11-05 00:45] falls asleep"
  , "[1518-11-05 00:55] wakes up"
  ]

partTwo guardData =
  guardData
  |> sort
  |> guards
  |> filterGuardsWhichArentSleeping
  |> map unpackShittyGuardFormat
  |> map calculateSleepingTime
  |> sort
  |> Map.fromAscListWith (++)
  |> Map.toList
  |> map (\(id, sleepingRange) ->
            sleepingRange
            |> mostCommon
            |> (\m -> (id, m, count m sleepingRange)))
  |> mostCommonMinuteSlept
  |> (\(id, minute, amount) -> id * minute)
  where mostCommonMinuteSlept (x:xs) = maxLen x xs
          where maxLen currentMax [] = currentMax
                maxLen (guardId, m, amount) ((nextGuard, nextMinute, nextAmount):ns)
                  | amount <  nextAmount  =
                      maxLen (nextGuard, nextMinute, nextAmount) ns
                  | otherwise = maxLen (guardId, m, amount) ns

dayFourMain :: IO ()
dayFourMain = do
  fileData <- getFile "inputs/DayFourInput"
  putStrLn $ show $ partOne fileData
  putStrLn $ show $ partTwo fileData

  pure ()
