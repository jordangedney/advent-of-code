-- | Things you're going to need.

module Util ( (&)
            , count
            , splitOn) where

import Data.Function ((&))
import Data.List.Split (splitOn)

count :: Eq a => a -> [a] -> Int
count x = length . filter (x ==)
