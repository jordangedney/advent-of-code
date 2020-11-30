module Util where

import System.IO
import Control.DeepSeq
import Debug.Trace

toInt :: String -> Int
toInt x = (read x :: Int)

traceThis :: (Show a) => a -> a
traceThis x = trace (show x) x

getFileStr :: FilePath -> IO String
getFileStr fileName =
  withFile fileName ReadMode $ \handle -> do
     fileString <- hGetContents handle
     return $!! fileString

getFile :: FilePath -> IO [String]
getFile fileName = getFileStr fileName >>= return . lines

infixl 0 |>
(|>) :: a -> (a -> c) -> c
(|>) = flip ($)

count :: Eq a => a -> [a] -> Int
count x = length . filter (x==)

apply numberOfTimes funct = foldr (.) id $ replicate numberOfTimes funct

cartesianProduct xs ys = [(x,y) | x <- xs, y <- ys]

enumerate :: [a] -> [(Int, a)]
enumerate = zip [0..]

digits :: Int -> [Int]
digits x =
  if x `div` 10 > 0
  then (digits $ x `div` 10) ++ [x `mod` 10]
  else [x `mod` 10]
