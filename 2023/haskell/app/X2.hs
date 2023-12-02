import Lude

data Color = R | G | B deriving (Eq, Show, Ord)

draw :: Parser (Color, Int)
draw = do
  amt <- read <$> some digitChar
  c <- choice [c <$ string s | (c, s) <- [(R, " red"), (G, " green"), (B, " blue")]]
  _ <- optional $ string ", "
  pure $ (c, amt)

getRound = do
  cs <- some draw
  _ <- optional $ string "; "
  pure cs

game = do 
  _ <- string "Game " >> some digitChar >> string ": "
  rs <- some getRound
  pure rs

valid (R, x) = x <= 12
valid (G, x) = x <= 13
valid (B, x) = x <= 14

main :: IO ()
main = do
  let input = 
        [ "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green"
        , "Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue"
        , "Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red"
        , "Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red"
        , "Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green" ]
  input <- lines <$> readFile "inputs/2"

  -- part one
  let parsed = map (parse game "") input & rights
      validGames = (map (and . (map (all valid)))) parsed 
      validIds = [id | (id, isValid) <- zip [1..] validGames, isValid]
  print $ sum validIds

  -- part two
  let getMax c xs = filter ((== c) . fst) xs & map snd & maximum
      highestDice xs = [getMax c xs | c <- [R, G, B]]
  print $ parsed & map concat & map (product . highestDice) & sum