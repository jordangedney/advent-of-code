-- | How did he know I travel with a shiny gold bag?

module Seven where

import Util
import Data.List (isInfixOf)

main :: IO ()
main = do
  input <- parseBagRules <$> readFile "inputs/seven"
  print (partOne input)
  print (partTwo input)

parseBagRules input =
  lines input
  & map (concat . splitOn ".")
  & map (concat . splitOn "bags")
  & map (concat . splitOn "bag")
  & map (unwords . words)
  & map (splitOn " contain ")
  & map (\[x, y] -> (x, y))
  & map (\(x, y) -> (x, splitOn " , " y))

partOne input =
  let getContainers color = [(a, b) | (a, b) <- input, any (color `isInfixOf`) b]
      go toFind =
        let colors = getContainers toFind & map fst
        in  colors ++ (concat (map go colors))
  in go "shiny gold" & unique & length

partTwo input =
  let getContainers color = [x | (a, b) <- input, x <- b, a == color]
      parseAmounts colors = [(read a :: Int, b ++ " " ++ c) | [a, b, c] <- map words colors]
      go toFind =
        let colors = getContainers toFind
        in if colors == ["no other"]
           then 1
           else 1 + sum [amount * go color | (amount, color) <- parseAmounts colors]
  in go "shiny gold" + (-1)
