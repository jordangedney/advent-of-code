module Lude ( module P
            , module Text.Megaparsec
            , module Text.Megaparsec.Char
            , module Control.Lens
            , module Debug.Trace

            , map
            , entry
            , ws
            , parseWithMap
            , numOf
            , frequencies
            , insertAt

            , Grid
            , Coord
            , mkGrid
            , right
            , left
            , up
            , down
            , cross
            , corners
            , adjacents
            , printMapWithModifier
            , printCharMap
            , undoParse
            , bfs
            , shortestPath

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
            , chunksOf
            , comparing
            , maximumBy

            , Parser
            , Void) where

import Prelude as P hiding (map)
import Text.Megaparsec hiding (noneOf)
import Text.Megaparsec.Char
import Control.Lens
import Debug.Trace

import qualified Data.Map.Strict as Map
-- import qualified Data.Vector as Vector
import qualified Data.Set as Set

import Data.Void (Void)

-- import Data.Function ((&))
import Control.Monad ((>=>))
import Data.List (transpose, nub)
import Data.Sort (sort)
import Data.Maybe (catMaybes, mapMaybe, fromJust)
import Text.Read (readMaybe)
import Data.Either (rights, lefts)
import Data.List.Split (chunksOf, splitOn)
import Data.Ord (comparing)
import Data.Foldable (maximumBy)

type Parser = Parsec Void String

map :: Functor f => (a -> b) -> f a -> f b
map = fmap

readInt :: String -> Maybe Int
readInt = readMaybe

entry :: Parser Int
entry = read <$> some digitChar <* many (char ' ')

ws :: Parser String
ws = many $ char ' '

parseWithMap m = choice [ p <$ char c | (p, c) <- m ]

numOf :: Eq a => a -> [a] -> Int
numOf x xs = filter (== x) xs & length

frequencies :: (Foldable t, Ord a) => t a -> Map.Map a Int
frequencies = foldr (\l r -> Map.insertWith (+) l 1 r) Map.empty

insertAt :: a -> [a] -> Int -> [a]
insertAt x ys i = case splitAt i ys of
    (before, after) -> before ++ [x] ++ after

takeUntilRepeat :: Ord a => [a] -> a
takeUntilRepeat xs = go Set.empty xs
  where go seen (x:xs) =
          if Set.member x seen then x else go (Set.insert x seen) xs


-- Grid code -------------------------------------------------------
type Coord = (Int, Int)
type Grid a = Map.Map Coord a

mkGrid :: [[a]] -> Grid a
mkGrid xss = Map.fromList
  [((x, y), c) | (x, i) <- zip [0..length xss] xss,
                 (y, c) <- zip [0..] i]

right (x, y) = (x, y + 1)
left  (x, y) = (x, y - 1)
up    (x, y) = (x - 1, y)
down  (x, y) = (x + 1, y)

cross   x = [f x | f <- [right, left, up, down]]
corners x = [f x | f <- [right . up, right . down, left . up, left . down]]
adjacents x = cross x ++ corners x

printMapWithModifier fn m =
  let len = last [y | ((x, y), _) <- Map.toAscList m, x == 0]
      chunked = chunksOf (len + 1) (Map.toAscList m)
      asString = map (concatMap fn) chunked
  in putStrLn "" >> mapM_ putStrLn asString

printCharMap :: Grid Char -> IO ()
printCharMap = printMapWithModifier (\x -> [snd x])

undoParse' :: Eq a => [(a, Char)] -> a -> Char
undoParse' ts x = head [z | (y, z) <- ts, x == y]

undoParse ts = (:[]) . undoParse' ts . snd

-- printShowValMap = printMapWithModifier (\(_, v) -> show v ++ " ")

bfs :: Grid a -> ((Int, Int) -> [(Int, Int)]) -> (Int, Int) -> (Int, Int) -> [(Int, Int)]
bfs grid getAdj start end = go [start] (Set.singleton start) []
  where go [] _ _ = []
        go (p:ps) visited path = if p == end then reverse (p:path) else
          let neighbors = [n | n <- getAdj p, Set.notMember n visited, Map.member n grid]
              newVisited = foldr Set.insert visited neighbors
          in go (ps ++ neighbors) newVisited (p:path)

shortestPath :: Grid a -> ((Int, Int) -> [(Int, Int)]) -> (Int, Int) -> (Int, Int) -> [(Int, Int)]
shortestPath grid getAdj start end = go [(start, Nothing)] Set.empty Map.empty
  where go [] _ _ = []
        go ((p, prev):ps) visited predecessors
          | p == end = reverse $ reconstructPath predecessors p
          | Set.member p visited = go ps visited predecessors
          | otherwise =
              let neighbors = [(n, Just p) | n <- getAdj p, Set.notMember n visited, Map.member n grid]
                  newVisited = Set.insert p visited
                  newPredecessors = foldr (\(n, p') acc -> Map.insert n p' acc) predecessors neighbors
              in go (ps ++ neighbors) newVisited newPredecessors

        reconstructPath predecessors node = case Map.lookup node predecessors of
          Just prev -> node : reconstructPath predecessors (fromJust prev)
          Nothing   -> [node]

--------------------------------------------------------------------

-- (|>) :: (a -> b) -> (b -> c) -> a -> c
-- (|>) f g a = g (f a)