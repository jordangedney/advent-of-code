import Lude

number :: Parser Int
number = do
  n <- optional $ char '-'
  e <- entry <* ws
  if n == Just '-' then pure (-1 * e) else pure e

main :: IO ()
main = do
  let input = testInput
  input <- lines <$> readFile "inputs/9"

  -- part one
  let parsed = map (parse (some number) "") input & rights
      differences xs = [b - a | (a, b) <- zip xs (tail xs)]
      getNext xs = 
        if all (== 0) (differences xs) then head xs
        else last xs + getNext (differences xs)
 
  print $ map getNext parsed & sum
 
  -- part two
  let getPrev xs = 
        if all (== 0) (differences xs) then head xs
        -- else traceShow (xs) $ last xs + getNext (differences xs)
        else head xs - getPrev (differences xs)

  print $ map getPrev parsed & sum

testInput = 
  [ "0 3 6 9 12 15"
  , "1 3 6 10 15 21"
  , "10 13 16 21 30 45" ]