{-# language NoImplicitPrelude #-}
{-# language OverloadedLists #-}

module Lude
  ( module P
  , module Data.Char
  , Map
  , Set
  , Vector
  , Point
  , Grid
  , mkGrid
  , adjacents
  , neighbors
  , positivePoints
  , map
  , rotate
  , splitOn
  , frequencies
  , range
  , count
  , sort
  , nub
  , fromDigits
  , readInt
  , catMaybes
  , mApply
  , M.findWithDefault
  , doFixed
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
import Data.Maybe (catMaybes)

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

frequencies :: (Foldable t, Ord a) => t a -> Map a Int
frequencies = foldr (\l r -> M.insertWith (+) l 1 r) []

range :: Int -> Int -> [Int]
range a b | a > b = reverse [b..a]
          | otherwise = [a..b]

count :: [Int] -> Int -> Int
xs `count` x = length (filter (== x) xs)

fromDigits :: [Int] -> Int
fromDigits = foldl (\n d -> 10*n + d) 0

readInt :: String -> Int
readInt = read

takeWhileUnique :: (Eq a) => [a] -> [a]
takeWhileUnique ys = go [] ys
  where go seen (x:xs) = if x `elem` seen then [] else x : (go (x:seen)) xs

doFixed :: (Eq a) => (a -> a) -> a -> [a]
doFixed fn start = takeWhileUnique $ iterate fn start

-- Grid Code: ------------------------------------------------------------------
type Point = (Int, Int)

type Grid = Map Point Int

mkGrid :: [String] -> Grid
mkGrid xs = map (map (readInt . (:[]))) xs
  |> zip [0..]
  |> map (\(a, ys) -> zip [0..] ys |> map (\(b, v) -> ((a, b), v)))
  |> concat
  |> M.fromList

-- has diagonals
adjacents :: Point -> [Point]
adjacents (a, b) = [(x, y) | x <- [a-1..a+1], y <- [b-1..b+1], (a, b) /= (x, y)]

neighbors :: Point -> [Point]
neighbors (x, y) = [(x, y - 1), (x + 1, y), (x, y + 1), (x - 1, y)]

-- If your grid is a topleft to bottomright style:
positivePoints :: [Point] -> [Point]
positivePoints xs = [a | a@(x, y) <- xs, x >= 0, y >= 0]

mApply :: (Ord a) => (b -> b) -> [a] -> Map a b -> Map a b
mApply fn xs g = foldr (\p m -> M.adjust fn p m) g xs
--------------------------------------------------------------------------------
