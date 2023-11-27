import Lude

main :: IO ()
main = do
  input <- readFile "inputs/1"

  -- part one
  let parse '(' = 1
      parse ')' = -1
      parse  _  = 0
  print (input & map parse & sum)

  -- part two
  print (input & map parse & scanl1 (+) & takeWhile (>= 0) & length & (+ 1))
