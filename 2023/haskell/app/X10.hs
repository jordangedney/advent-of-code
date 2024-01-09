import Lude
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.List (sortBy)
import Data.Ord (comparing)
import Data.Foldable (maximumBy)

data Pipe = NS | EW | NE | NW | SW | SE | GG | SS
  deriving (Show, Eq, Ord)

pipe :: Parser Pipe
pipe = choice [ p <$ char c | (p, c) <-
  [ (NS, '|'), (EW, '-'), (NE, 'L'), (NW, 'J'),
    (SW, '7'), (SE, 'F'), (GG, '.'), (SS, 'S') ] ]

main :: IO ()
main = do
  let input = testInput
  input <- lines <$> readFile "inputs/10"

  -- part one
  let parsed = map (parse (some pipe) "") input & rights & mkGrid
      start = maximumBy (comparing snd) (parsed & Map.toList) & fst
      l v c = Map.findWithDefault GG c parsed `elem` v
      validAbove = l [NS, SW, SE]
      validBelow = l [NS, NE, NW]
      validRight = l [EW, NW, SW]
      validLeft  = l [EW, NE, SE]
      getters = [(up, validAbove), (down, validBelow), (right, validRight), (left, validLeft)]
      getNext c = [g c | (g, v) <- getters, v (g c)]

      followThePipe haveBeen toGo = 
        let nextNodes = concatMap getNext toGo 
                        & filter (not . (`elem` haveBeen))
                        & filter ((> 0) . length . getNext)
        in if null nextNodes then nub haveBeen
           else followThePipe (nextNodes ++ haveBeen) nextNodes

  print $ shortestPath parsed getNext start (followThePipe [] [start] & head) & length & (+ (-1))

  -- part two

testInput =
  [ "..F7."
  , ".FJ|."
  , "SJ.L7"
  , "|F--J"
  , "LJ..." ]

testInput2 =
  [ ".|..."
  , ".S-7."
  , ".|.|."
  , ".L-J."
  , "....." ]