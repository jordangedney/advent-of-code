import Data.List.Split (splitOn)
import Data.Sort (sort)

parse :: String -> [Int]
parse = map (sum . map read . lines) . splitOn "\n\n"

main :: IO ()
main = do
  input <- parse <$> readFile "../inputs/1"

  -- part one
  print (foldr1 max input)

  -- part two
  print ((sum . take 3 . reverse . sort) input)
