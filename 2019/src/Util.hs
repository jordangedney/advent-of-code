module Util
  ( splitOn
  , replace
  , sortBy
  , sort
  , tracer
  , unique
  , digits
  , count
  , Coord
  , manhattan
  )
where

import Data.List.Split (splitOn)
import Data.List (sortBy, sort)
import Debug.Trace
import qualified Data.Set as Set

unique :: Ord a => [a] -> [a]
unique = go Set.empty where
  go s (x:xs)
   | x `Set.member` s = go s xs
   | otherwise        = x : go (Set.insert x s) xs
  go _ _              = []

tracer :: Show a1 => [a1] -> a2 -> a2
tracer xs = trace (unlines $ map show xs)

replace :: Int -> a -> [a] -> [a]
replace n element xs = let (x,_:ys) = splitAt n xs in x ++ element : ys

digits :: Integral x => x -> [x]
digits 0 = []
digits x = digits (x `div` 10) ++ [x `mod` 10]

count :: Eq a => a -> [a] -> Int
count x = length . filter (x ==)

type Coord = (Int, Int)

manhattan :: Coord -> Coord -> Int
manhattan (x, y) (a, b) = abs (x - a) + abs (y - b)
