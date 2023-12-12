{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use zipWith" #-}
import Lude
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Maybe (fromJust)

galacticMap :: [[Char]] -> Grid Char
galacticMap input = 
  let (rowLen, colLen) = (head input & length, length input)
      withExtraRows = 
        let emptyRows = [i | (i, r) <- zip [0..] input, all (== '.') r ]
              -- add [+0, +1, +2..] to help the fold account for the offset of new rows
              & zip [0..] & map (uncurry (+)) 
        in foldl (insertAt (replicate rowLen '.')) input emptyRows

      withExtraCols = 
        let emptyCols = [i | (i, r) <- zip [0..] (transpose input), all (== '.') r ]
              & zip [0..] & map (uncurry (+)) 
            insertCol cs i = [insertAt '.' c i | c <- cs]
        in foldl insertCol withExtraRows emptyCols
  in mkGrid withExtraCols

main :: IO ()
main = do
  let input = testInput & galacticMap
  -- input <- galacticMap . lines <$> readFile "inputs/11"

  -- part one
  printMap input

  putStrLn "\n"
  printMap $ foldr (`Map.insert` 'X') input (shortestPath input cross (0, 4) (11, 5)) 

testInput :: [String]
testInput =
  [ "...#......"
  , ".......#.."
  , "#........."
  , ".........."
  , "......#..."
  , ".#........"
  , ".........#"
  , ".........."
  , ".......#.."
  , "#...#....." ]