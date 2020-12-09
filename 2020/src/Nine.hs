-- | Don't read; bleh.

module Nine where

import Util
-- import Data.List.Split (chunksOf)
import Data.List (sort)

main :: IO ()
main = do
  input <- map readInt . lines <$> readFile "inputs/nine"
  --print (partOne input)
  print (partTwo input)
  return ()

readInt :: String -> Int
readInt = read

overLappingChunks size input =
  let result = take size input
  in if length result < size
     then []
     else result : overLappingChunks size (tail input)

partOne input =
  let size = 25
      preamble = overLappingChunks size input
      possible = map (\x -> [y + z | y <- x, z <- x]) preamble
      input' = drop size input
  in  filter (\(x, y) -> not (y `elem` x)) (zip possible input') & head & snd

partTwo input =
  let answer = 22406676
      --answer = 127
      go chunkSize =
        let res = overLappingChunks chunkSize input
            withSum = map (\x -> (sum x, x)) res
            any' = [x | (s, x) <- withSum, s == answer]
        in case any' of
          [] -> go (chunkSize + 1)
          _ -> any'
      res = sort (head (go 2))
  in (foldr1 min res) + (foldr1 max res)

-- partTwo input = undefined
