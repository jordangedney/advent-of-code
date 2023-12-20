import Lude
import qualified Data.Map.Strict as M
import qualified Data.Set as S

data Template = E | LM deriving (Show, Eq, Ord)

part :: Parser Template
part = choice [ p <$ char c | (p, c) <-
  [ (E, '.'), (LM, '/') ] ]

main :: IO ()
main = do
  let input = testInput
  input <- lines <$> readFile "inputs/17"

  -- part one
  let parsed = map (parse (some part) "") input & rights
  print parsed

  -- part two
  print parsed

testInput =
  [ "2413432311323"
  , "3215453535623"
  , "3255245654254"
  , "3446585845452"
  , "4546657867536"
  , "1438598798454"
  , "4457876987766"
  , "3637877979653"
  , "4654967986887"
  , "4564679986453"
  , "1224686865563"
  , "2546548887735"
  , "4322674655533" ]