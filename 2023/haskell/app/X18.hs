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
  -- input <- lines <$> readFile "inputs/18"

  -- part one
  let parsed = map (parse (some part) "") input & rights
  print parsed

  -- part two
  print parsed

testInput =
  [ "R 6 (#70c710)"
  , "D 5 (#0dc571)"
  , "L 2 (#5713f0)"
  , "D 2 (#d2c081)"
  , "R 2 (#59c680)"
  , "D 2 (#411b91)"
  , "L 5 (#8ceee2)"
  , "U 2 (#caa173)"
  , "L 1 (#1b58a2)"
  , "U 2 (#caa171)"
  , "R 2 (#7807d2)"
  , "U 3 (#a77fa3)"
  , "L 2 (#015232)"
  , "U 2 (#7a21e3)" ]