-- | Things you're going to need.

module Util ( (&)
            , count
            , unique
            , splitOn) where

import Data.Function ((&))
import Data.List.Split (splitOn)
import qualified Data.Set as Set

count :: Eq a => a -> [a] -> Int
count x = length . filter (x ==)

unique :: Ord a => [a] -> [a]
unique = go Set.empty where
  go s (x:xs)
   | x `Set.member` s = go s xs
   | otherwise        = x : go (Set.insert x s) xs
  go _ _              = []
