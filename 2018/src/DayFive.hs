module DayFive where

import Util (getFileStr, (|>), )
import Data.Char (toLower)

react polymer = go [] polymer
  where go alreadySeen@(a:as) (u1:u2:compound) =
          if reactive u1 u2
          then go (init alreadySeen) ((last alreadySeen):compound)
          else go (alreadySeen ++ [u1]) (u2:compound)
        go [] (u1:u2:compound) =
          if reactive u1 u2
          then go [] compound
          else go [u1] (u2:compound)
        go alreadySeen [x] = alreadySeen ++ [x]
        reactive u1 u2 = and [toLower u1 == toLower u2, u1 /= u2]

remove removed xs = [x | x <- xs, x /= removed]

partOne = length . react

partTwo fileData =
  zip ['a'..'z'] ['A'..'Z']
  |> map (\(l, r) -> [x | x <- fileData, x /= l, x /= r])
  |> map (length . react)
  |> foldl1 min

dayFiveMain :: IO ()
dayFiveMain = do
  fileData <- getFileStr "inputs/DayFiveInput"
  putStrLn $ show $ partOne fileData
  putStrLn $ show $ partTwo fileData
  pure ()
