module X10 (main) where

import Lude
import qualified Data.Map as M
import Control.Monad (forM_)

data Symbol = LParen | RParen | LSqrBr | RSqrBr | LCrlBr | RCrlBr | LAngBr | RAngBr
  deriving (Show, Eq)

parse = map $ \case
  '(' -> LParen
  ')' -> RParen
  '[' -> LSqrBr
  ']' -> RSqrBr
  '{' -> LCrlBr
  '}' -> RCrlBr
  '<' -> LAngBr
  '>' -> RAngBr

removeInner [] = []
removeInner (LParen:RParen:xs) = removeInner xs
removeInner (LSqrBr:RSqrBr:xs) = removeInner xs
removeInner (LCrlBr:RCrlBr:xs) = removeInner xs
removeInner (LAngBr:RAngBr:xs) = removeInner xs
removeInner (x:xs) = x : removeInner xs

doFixed fn input = input : go [input] input
  where go seen in' =
          let r = fn in'
          in if r `elem` seen then [] else r : go (r:seen) r

score = \case
  LParen -> 3
  RParen -> 3
  LSqrBr -> 57
  RSqrBr -> 57
  LCrlBr -> 1197
  RCrlBr -> 1197
  LAngBr -> 25137
  RAngBr -> 25137

score' = \case
  LParen -> 1
  RParen -> 1
  LSqrBr -> 2
  RSqrBr -> 2
  LCrlBr -> 3
  RCrlBr -> 3
  LAngBr -> 4
  RAngBr -> 4

finalScore s [] = s
finalScore s (x:xs) = finalScore (s * 5 + (score' x)) xs

part1 xs =
  map (doFixed removeInner) xs
  |> map last
  |> map (filter (`elem` [RParen, RSqrBr, RCrlBr, RAngBr]))
  |> filter (not . null)
  |> map (score . head)
  |> sum

part2 xs =
  map (doFixed removeInner) xs
  |> map last
  |> filter (\ys -> not $ foldr1 (||) (map (`elem` [RParen, RSqrBr, RCrlBr, RAngBr]) ys))
  |> map reverse
  |> map (finalScore 0)
  |> sort
  |> (\x -> x !! ((length x - 1) `div` 2))


main = readFile "inputs/10" <&> lines >>> map parse >>> part2 >>= print

printLst xs = do
  forM_ xs $ \x -> do
    print x

-- printSym xs = map toChar xs >>= print
toChar = \case
  LParen -> '('
  RParen -> ')'
  LSqrBr -> '['
  RSqrBr -> ']'
  LCrlBr -> '{'
  RCrlBr -> '}'
  LAngBr -> '<'
  RAngBr -> '>'
