import Lude
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust)

network :: Parser (String, (String, String))
network = do
  start <- some alphaNumChar <* " = ("
  left <- some alphaNumChar <* ", "
  right <- some alphaNumChar <* ")"
  pure (start, (left, right))

data Dir = L | R deriving (Show, Eq)

main :: IO ()
main = do
  let input = input2
  input <- lines <$> readFile "inputs/8"

  -- part one
  let directions = head input & map (\case 'R' -> R; _ -> L )
      parsed = map (parse network "") (drop 2 input) & rights & Map.fromList

      move dir m i = Map.lookup i m & fromJust & (case dir of L -> fst; R -> snd;)

      go (d:ds) m pos goal steps = 
        if pos == goal then steps
        else go ds m (move d m pos) goal (steps + 1)

  print $ go (cycle directions) parsed "AAA" "ZZZ" 0

  -- part two
  let starts = Map.keys parsed & filter ((== 'A') . last)

      followPath start = scanl (\pos dir -> move dir parsed pos) start (cycle directions)

      stepsUntilZNodes start = followPath start & zip [1..] & filter (\(_, y) -> last y == 'Z')

      posOfStartAndEndOfCycle = map (map fst . take 2 . stepsUntilZNodes) starts
      stepsInCycle = [b - a | [a, b] <- posOfStartAndEndOfCycle]

  print $ foldl1 lcm stepsInCycle

input1 =
  [ "RL"
  , ""
  , "AAA = (BBB, CCC)"
  , "BBB = (DDD, EEE)"
  , "CCC = (ZZZ, GGG)"
  , "DDD = (DDD, DDD)"
  , "EEE = (EEE, EEE)"
  , "GGG = (GGG, GGG)"
  , "ZZZ = (ZZZ, ZZZ)" ]
input2 = [ "LR"
  , ""
  , "11A = (11B, XXX)"
  , "11B = (XXX, 11Z)"
  , "11Z = (11B, XXX)"
  , "22A = (22B, XXX)"
  , "22B = (22C, 22C)"
  , "22C = (22Z, 22Z)"
  , "22Z = (22B, 22B)"
  , "XXX = (XXX, XXX)" ]