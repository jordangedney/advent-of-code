import Lude
import qualified Data.Map.Strict as Map
import Data.List (sortBy)
import Data.Ord (comparing)
import Data.Foldable (maximumBy)
import Data.Maybe (fromJust)
import Data.List.Split (chunksOf)

data Pipe = NS | EW | NE | NW | SW | SE | GG | SS
  deriving (Show, Eq, Ord)

pipe :: Parser Pipe
pipe = choice [ p <$ char c | (p, c) <-
  [ (NS, '|'), (EW, '-'), (NE, 'L'), (NW, 'J'),
    (SW, '7'), (SE, 'F'), (GG, '.'), (SS, 'S') ] ]

main :: IO ()
main = do
  let input = testInput2
  -- input <- lines <$> readFile "inputs/10"

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
      takeUntilRepeat ys = tUR ys []
        where tUR (x:xs) seen = if x `elem` seen then reverse seen else tUR xs (x:seen)
      followThePipe haveBeen toGo = 
        let nextNodes = concatMap getNext toGo 
                        & filter (not . (`elem` haveBeen))
        in if null nextNodes then nub haveBeen
           else followThePipe (nextNodes ++ haveBeen) nextNodes

      bfPaths :: (a -> [a]) -> a -> [[a]]
      bfPaths step seed  =  go [ (seed, [seed]) ]
       where
       go []  =  []
       go ((s, path) : q) = path :
         go (q ++ [ (x, path ++ [x]) | x <- step s ])


      -- printMap :: Grid Pipe -> [[Pipe]]
      printMap m =
        let len = last [y | ((x, y), _) <- Map.toAscList m, x == 0]
            chunked = chunksOf (len + 1) (Map.toAscList m)
        --in map (map show) chunked
        in map (map snd) chunked
        
  -- mapM_ print $ printMap parsed
  
  let f = bfPaths getNext start &  map (length . nub)  & take 1000
  -- print (parsed & length)
  -- print start
  -- print $ followThePipe [start] [start]
  print $ followThePipe [] [start] & sort & length & (+ 1)
  print f
  -- mapM_ print $ printMap (foldl (\m k -> Map.insert k SS m) parsed (followThePipe [start] [start]))
  -- mapM_ print $ printMap (foldl (\m k -> Map.insert k SS m) parsed (f))

  -- print (getNext start)
  -- print $ parsed
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