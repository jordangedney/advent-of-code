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
  -- input <- lines <$> readFile "inputs/25"

  -- part one
  let parsed = map (parse (some part) "") input & rights
  print parsed

  -- part two
  print parsed

testInput =