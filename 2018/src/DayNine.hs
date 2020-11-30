{-# LANGUAGE BangPatterns #-}
module DayNine where

import Util ((|>), apply)
import Data.List (sort, foldl')
import qualified Data.Map as Map

data CircularList a = CircularList [a] [a] deriving (Show, Eq)

circularToList (CircularList prev next) = reverse prev ++ next

circularFromList range = foldr (\l r -> ins l r) (CircularList [] []) (reverse range)

pop (CircularList [] next) = pop $ CircularList (reverse next) []
pop (CircularList (p:prev) next) = (p, CircularList prev next)

peek (CircularList [] next) = peek $ CircularList (reverse next) []
peek (CircularList (p:prev) next) = p

skip (CircularList prev []) = skip $ CircularList [] (reverse prev)
skip (CircularList prev (n:next)) = CircularList (n:prev) next

prev (CircularList [] next) = prev $ CircularList (reverse next) []
prev (CircularList (p:prev') next) = CircularList prev' (p:next)

ins element (CircularList prev next) = CircularList (element:prev) next

marbleGame :: (Int, Int) -> [(Int, Int)]
marbleGame (numPlayers, numMarbles) = fst $ foldl' go emptyGame playersAndMarbles
  where emptyGame = ([], CircularList [0] [])
        playersAndMarbles = zip (cycle [1..numPlayers]) [1..numMarbles]
        popSeventhPrevious = (\(x, y) -> (x, (skip . skip) y)) . pop . (apply 8 prev)
        insertMarble marbles = skip . ins marbles
        go (scores, !marbles) (curPlayer, curMarble) =
          if curMarble `mod` 23 == 0 then
            let (popped, nMarbles) = popSeventhPrevious marbles
            in (((curPlayer, popped + curMarble):scores), nMarbles)
          else (scores, insertMarble curMarble marbles)

partOne game =
  marbleGame game
  |> Map.toList . (Map.fromAscListWith (+)) . sort
  |> map snd
  |> foldl1 max

partTwo (players, numMarbles) = partOne (players, numMarbles * 100)

dayNineMain :: IO ()
dayNineMain = do
  let fileData = (424, 71144)
  putStrLn $ show $ partOne fileData
  putStrLn $ show $ partTwo fileData

  pure ()
