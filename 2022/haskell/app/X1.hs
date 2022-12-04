import Data.List.Split (splitOn)
import Data.Sort (sort)

parse = map (sum . map read . lines) . splitOn "\n\n"
partOne = foldr1 max
partTwo = sum . take 3 . reverse . sort

main :: IO ()
main = do
  input <- parse <$> readFile "../inputs/1"
  print (partOne input)
  print (partTwo input)
