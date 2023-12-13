import Lude
import Control.Monad (join)
import Data.List (intercalate)

data S = O | D | U deriving (Show, Eq)

spring :: Parser ([S], [Int])
spring = do
  ss <- some $ choice [s <$ char c | (s, c) <- [(O, '.'), (D, '#'), (U, '?')]]
  ws
  rs <- some $ entry <* optional (char ',')
  pure (ss, rs)

validConditionRecord (states, records) =
  notElem U states
  && (records == (splitOn [O] states & filter (not . null) & map length))

replaceFirst _ _ [] = []
replaceFirst old new (x:xs)
  | x == old = new : xs
  | otherwise = x : replaceFirst old new xs

differentArrangements xs =
  if U `notElem` xs then [xs]
  else concatMap differentArrangements [replaceFirst U O xs, replaceFirst U D xs]

main :: IO ()
main = do
  let input = testInput
  -- input <- lines <$> readFile "inputs/12"

  -- part one
  let parsed = map (parse spring "") input & rights
      solve xs = [(nss, rs) | (ss, rs) <- xs
                            , nss <- differentArrangements ss
                            , validConditionRecord (nss, rs)]
  print $ solve parsed & length

  -- part two
  let unfolded = [(intercalate [U] $ replicate 5 ss, concat $ replicate 5 rs) | (ss, rs) <- parsed]
  print $ unfolded !! 1
  mapM_ print $ differentArrangements (fst $ unfolded !! 1) 
  -- print $ solve [unfolded !! 1] & length


testInput :: [String]
testInput =
  [ "???.### 1,1,3"
  , ".??..??...?##. 1,1,3"
  , "?#?#?#?#?#?#?#? 1,3,1,6"
  , "????.#...#... 4,1,1"
  , "????.######..#####. 1,6,5"
  , "?###???????? 3,2,1" ]