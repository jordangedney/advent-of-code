module DayEight where

import Util (getFileStr, (|>))
import Data.Maybe (catMaybes)
import Safe

toInts = map (\x -> (read x :: Int)) . words
testData = toInts "2 3 0 3 10 11 12 1 1 0 1 99 2 1 1 2"

data Tree a = Leaf a | Branch a [Tree a] deriving (Show, Eq)

parseTree = fst . parseTree'
parseTree' (0:numMeta:toParse) = (Leaf (take numMeta toParse), drop numMeta toParse)
parseTree' (numChildren:numMeta:toParse) =
  parseChildren numChildren toParse []
  |> (\(children, leftover) ->
        (Branch (take numMeta leftover) children, drop numMeta leftover))
  where parseChildren 0 toParse children = (children, toParse)
        parseChildren numChildren toParse children =
          parseTree' toParse
          |> (\(child, leftover) ->
                parseChildren (numChildren - 1) leftover (children ++ [child]))

preorder (Leaf xs) = xs
preorder (Branch xs leaves) = xs ++ concatMap preorder leaves

value (Leaf xs) = sum xs
value (Branch xs leaves) = sum $ map value $ metaLeaves
  where metaLeaves = catMaybes $ map (Safe.atMay leaves) $ map ((+) (-1)) xs

partOne = sum . preorder . parseTree
partTwo = value . parseTree

dayEightMain :: IO ()
dayEightMain = do
  fileData <- getFileStr "inputs/DayEightInput" >>= pure . toInts
  putStrLn $ show $ partOne fileData
  putStrLn $ show $ partTwo fileData

  pure ()
