import Lude

main :: IO ()
main = do
  input <- readFile "inputs/3"

  -- part one
  let walk (x, y) = \case
        '>' -> (x + 1, y)
        '<' -> (x - 1, y)
        '^' -> (x, y + 1)
        'v' -> (x, y - 1)
        _  ->  (x, y)
  print $ scanl walk (0, 0) input & nub & length

  -- part two
  let dosSantos (santa, robo, fatMansTurn) n = 
        if fatMansTurn then (walk santa n, robo, False) 
        else (santa, walk robo n, True)
  print $ scanl dosSantos ((0, 0), (0, 0), True) input 
        & concatMap (\(sPos, rPos, _) -> [sPos, rPos])
        & nub & length