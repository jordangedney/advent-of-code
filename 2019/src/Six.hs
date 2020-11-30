module Six where

import Util (splitOn, unique)
import qualified Data.Map.Strict as Map

pathToRoot :: Map.Map String [String] -> String -> [String]
pathToRoot _ "COM" = []
pathToRoot m node = nextNode : pathToRoot m nextNode
  where nextNode = head [p | (p, vs) <- Map.toList m, node `elem` vs]

pathTo :: Map.Map String [String] -> String -> String -> [String]
pathTo m node target =
  let [toRootA, toRootB] = pathToRoot m <$> [node, target]
      firstCommonNode = head [n | n <- toRootA, n `elem` toRootB]
      [pathToCommonA, pathToCommonB] = takeWhile (/= firstCommonNode) <$> [toRootA, toRootB]
  in pathToCommonA ++ [firstCommonNode] ++ reverse pathToCommonB

partOne :: Map.Map String [String] -> Int
partOne m = sum $ map (length . pathToRoot m) (unique . concat $ Map.elems m)

partTwo :: Map.Map String [String] -> Int
partTwo m = (+ (-1)) . length $ pathTo m "YOU" "SAN"

progMain :: IO ()
progMain = do
  input <- parse . fmap (splitOn ")") . lines <$> readFile "inputs/six"
  print $ partOne input
  print $ partTwo input

parse ::[[String]] -> Map.Map String [String]
parse = foldl (\result [x, y] -> Map.insertWith (++) x [y] result) Map.empty
