import Data.Functor
import Data.Function ((&))

data Move = Rock | Paper | Scissors | Lose | Draw | Win deriving (Eq, Enum, Show)

main :: IO ()
main = do
  let translate m = case m of
        "A" -> Rock
        "B" -> Paper
        "C" -> Scissors
        "X" -> Rock
        "Y" -> Paper
        "Z" -> Scissors
        _   -> error "parsing"

  input <- readFile "../inputs/2"
        <&> lines
        <&> map words
        <&> map (map translate)

  -- part one
  let win Scissors Rock  = True
      win Rock Paper     = True
      win Paper Scissors = True
      win _ _ = False

      scoreShape [_, x] = fromEnum x + 1

      scoreRound [x, y] | win x y = 6
                        | x == y = 3
                        | otherwise = 0

  print (sum (map (\r -> scoreShape r + scoreRound r) input))

  -- part two - Hooray for meaningless types!
  let translate m = case m of
        "A" -> Rock
        "B" -> Paper
        "C" -> Scissors
        "X" -> Lose
        "Y" -> Draw
        "Z" -> Win
        _   -> error "parsing"

  input <- readFile "../inputs/2"
        <&> lines
        <&> map words
        <&> map (map translate)

  let getMove opponents desiredOutcome =
        case desiredOutcome of
          Lose -> case opponents of
            Rock     -> Scissors
            Paper    -> Rock
            Scissors -> Paper
          Draw -> case opponents of
            Rock     -> Rock
            Paper    -> Paper
            Scissors -> Scissors
          Win -> case opponents of
            Rock     -> Paper
            Paper    -> Scissors
            Scissors -> Rock

  print $ map (\[x, y] -> [x, (getMove x y)]) input
          & map (\r -> scoreShape r + scoreRound r)
          & sum
