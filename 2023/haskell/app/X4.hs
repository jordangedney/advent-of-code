import Lude
import qualified Data.Map.Strict as Map

card :: Parser (Int, [Int], [Int])
card = do 
  string "Card" >> ws
  cardNum <- entry <* char ':' <* ws
  winningNumbers <- some entry
  char '|' >> ws
  chances <- some entry
  return (cardNum, winningNumbers, chances)

  where entry = read <$> some digitChar <* ws
        ws = many $ char ' '

main :: IO ()
main = do
  let input =
        [ "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53"
        , "Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19"
        , "Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1"
        , "Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83"
        , "Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36"
        , "Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11" ]
  input <- lines <$> readFile "inputs/4"

  -- part one
  let parsed = map (parse card "") input & rights
      onlyWinners (_, ws, ns) = filter (`elem` ws) ns
      winCount = length . onlyWinners
      scoreCards xs = sum [2 ^ (x - 1) | x <- map winCount xs, x /= 0]
  print $ scoreCards parsed

  -- part two
  let copies = map (\c@(i, _, _) -> (i, [i+1..i+winCount c])) parsed
      numCopies m k = 
        case Map.findWithDefault [] k m of
          [] -> 1 
          xs -> 1 + sum (map (numCopies m) xs)
  print $ sum [numCopies (Map.fromList copies) x | x <- [1..length copies]]