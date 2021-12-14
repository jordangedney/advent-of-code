module X10 (main) where

import Lude
import qualified Data.Map as M

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

removeInner = \case
  []                 -> []
  (LParen:RParen:xs) -> removeInner xs
  (LSqrBr:RSqrBr:xs) -> removeInner xs
  (LCrlBr:RCrlBr:xs) -> removeInner xs
  (LAngBr:RAngBr:xs) -> removeInner xs
  (x:xs)             -> x : removeInner xs

score1 = \case { RParen -> 3; RSqrBr -> 57; RCrlBr -> 1197; RAngBr -> 25137; }
score2 = \case { LParen -> 1; LSqrBr -> 2;  LCrlBr -> 3;    LAngBr -> 4;     }

part1 xs =
  map (doFixed removeInner) xs
  |> map last
  |> map (filter (`elem` [RParen, RSqrBr, RCrlBr, RAngBr]))
  |> filter (not . null)
  |> map (score1 . head)
  |> sum

part2 xs =
  map (doFixed removeInner) xs
  |> map last
  |> filter (\ys -> not $ foldr1 (||) (map (`elem` [RParen, RSqrBr, RCrlBr, RAngBr]) ys))
  |> map reverse
  |> map (foldl (\l r -> l * 5 + (score2 r)) 0)
  |> sort
  |> (\x -> x !! ((length x - 1) `div` 2))

main = readFile "inputs/10" <&> lines >>> map parse >>> part2 >>= print
