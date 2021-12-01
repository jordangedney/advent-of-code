{-# language NoImplicitPrelude #-}

module Prelude
  ( module P
  , module Data.Char
  , Map
  , Set
  , map
  , (|>)
  , (<|)
  , (>>>)
  , (<<<)
  )
where

import BasePrelude as P hiding (map)

import Data.Char
import Data.Map.Strict (Map)
import Data.Set (Set)

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

