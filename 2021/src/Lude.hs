{-# language NoImplicitPrelude #-}
{-# language OverloadedLists #-}

module Lude
  ( module P
  , module Data.Char
  , Map
  , Set
  , Vector
  , Point
  , map
  , rotate
  , splitOn
  , frequencies
  , range
  , sort
  , nub
  , M.findWithDefault
  , (|>)
  , (<|)
  , (>>>)
  , (<<<)
  , (<&>)
  )
where

import Prelude as P hiding (map)

import Data.Functor ((<&>))
import Data.Char
import Data.Map.Strict (Map)
import Data.Vector (Vector)
import Data.Set (Set)
import Data.List (transpose)
import Data.List.Split (splitOn)
import Data.Sort (sort)
import Data.List (nub)

import qualified Data.Map as M

map :: Functor f => (a -> b) -> f a -> f b
map = fmap

infixl 1 |>
(|>) :: a -> (a -> b) -> b
x |> f = f x
{-# INLINE (|>) #-}

infixr 0 <|
(<|) :: (a -> b) -> a -> b
(<|) = ($)
{-# INLINE (<|) #-}

infixr 9 <<<
(<<<) :: (b -> c) -> (a -> b) -> (a -> c)
g <<< f = g . f
{-# INLINE (<<<) #-}

infixl 9 >>>
(>>>) :: (a -> b) -> (b -> c) -> (a -> c)
f >>> g = g . f
{-# INLINE (>>>) #-}

rotate :: [String] -> [String]
rotate = reverse . transpose . map reverse

type Point = (Int, Int)

frequencies :: (Foldable t, Ord a) => t a -> Map a Int
frequencies = foldr (\l r -> M.insertWith (+) l 1 r) []

range :: Int -> Int -> [Int]
range a b | a > b = reverse [b..a]
          | otherwise = [a..b]

