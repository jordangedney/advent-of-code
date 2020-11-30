module DayOne where
import Data.IntSet (empty, member, insert)

fstDup seen (x:xs) = if x `member` seen then x else fstDup (insert x seen) xs
toInt = read . (filter (/= '+'))

dayOneMain :: IO ()
dayOneMain = do
  fileData <- fmap (map toInt . lines) $ readFile "inputs/DayOneInput"
  print [sum fileData, fstDup empty $ [0] ++ (scanl1 (+) $ cycle fileData)]
