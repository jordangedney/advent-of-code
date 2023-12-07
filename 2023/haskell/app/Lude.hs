module Lude ( module P
            , module Text.Megaparsec
            , module Text.Megaparsec.Char
            , module Control.Lens
            , module Debug.Trace

            , map
            , entry
            , ws
            , numOf
            , frequencies

            -- , (&)
            -- , (|>)
            , (>=>)
            , transpose
            , splitOn
            , sort
            , nub
            , catMaybes
            , readInt
            , mapMaybe
            , rights
            , lefts

            , Parser
            , Void) where

import Prelude as P hiding (map)
import Text.Megaparsec hiding (noneOf)
import Text.Megaparsec.Char
import Control.Lens
import Debug.Trace

import qualified Data.Map.Strict as Map
-- import qualified Data.Vector as Vector
-- import qualified Data.Set as Set

import Data.Void (Void)

-- import Data.Function ((&))
import Control.Monad ((>=>))
import Data.List (transpose, nub)
import Data.List.Split (splitOn)
import Data.Sort (sort)
import Data.Maybe (catMaybes, mapMaybe)
import Text.Read (readMaybe)
import Data.Either (rights, lefts)

type Parser = Parsec Void String

map :: Functor f => (a -> b) -> f a -> f b
map = fmap

readInt :: String -> Maybe Int
readInt = readMaybe

entry :: Parser Int
entry = read <$> some digitChar <* many (char ' ')

ws :: Parser String
ws = many $ char ' '

numOf :: Eq a => a -> [a] -> Int
numOf x xs = filter (== x) xs & length 

frequencies :: (Foldable t, Ord a) => t a -> Map.Map a Int
frequencies = foldr (\l r -> Map.insertWith (+) l 1 r) Map.empty

-- (|>) :: (a -> b) -> (b -> c) -> a -> c
-- (|>) f g a = g (f a)