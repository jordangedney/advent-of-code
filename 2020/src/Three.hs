-- |  ~yawn~

module Three where

import Util

main :: IO ()
main = do
  input <- map parseArea . lines <$> readFile "inputs/three"
  print (partOne input)
  print (partTwo input)

countSlope input slope =
  let traversed = [0,slope..] & map (`mod` (length (head input)))
      sled = zip input traversed & map (uncurry (!!))
  in count Tree sled

partOne input = countSlope input 3

partTwo input =
  let sled = map (countSlope input) [1, 3, 5, 7]
      imtired (x:y:[]) = [x]
      imtired (x:_:xs) = x: imtired xs
      imtired _ = []
      hack = countSlope (imtired input) 1
  in foldr (*) hack sled

data Area = Open | Tree deriving Eq

parseArea = map (\x -> if x == '.' then Open else Tree)
