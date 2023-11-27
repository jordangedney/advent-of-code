module Lude ( module P

            , map

            , (&)
            , (|>)
            , (>=>)
            , transpose
            , splitOn
            , sort
            , nub
            , catMaybes
            , readInt
            , mapMaybe

            , Map
            , Set
            , Vector) where

import Prelude as P hiding (map)

import Data.Map.Strict (Map)
import Data.Vector (Vector)
import Data.Set (Set)

import Data.Function ((&))
import Control.Monad ((>=>))
import Data.List (transpose, nub)
import Data.List.Split (splitOn)
import Data.Sort (sort)
import Data.Maybe (catMaybes, mapMaybe)
import Text.Read (readMaybe)

map :: Functor f => (a -> b) -> f a -> f b
map = fmap

readInt :: String -> Maybe Int
readInt = readMaybe

(|>) :: (a -> b) -> (b -> c) -> a -> c
(|>) f g a = g (f a)