module Lude ( module P

            , map

            , (&)
            , transpose
            , splitOn
            , sort
            , nub
            , catMaybes

            , Map
            , Set
            , Vector) where

import Prelude as P hiding (map)

import Data.Map.Strict (Map)
import Data.Vector (Vector)
import Data.Set (Set)

import Data.Function ((&))
import Data.List (transpose)
import Data.List.Split (splitOn)
import Data.Sort (sort)
import Data.List (nub)
import Data.Maybe (catMaybes)

map :: Functor f => (a -> b) -> f a -> f b
map = fmap
