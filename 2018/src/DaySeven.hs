module DaySeven where

import Util (getFile, (|>), count)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.List (sort)
import Data.Char (ord)
import Data.Maybe (catMaybes)
import Safe

testData =
  [ "Step C must be finished before step A can begin."
  , "Step C must be finished before step F can begin."
  , "Step A must be finished before step B can begin."
  , "Step A must be finished before step D can begin."
  , "Step B must be finished before step E can begin."
  , "Step D must be finished before step E can begin."
  , "Step F must be finished before step E can begin."
  ]

invert :: (Ord k, Ord a) => Map.Map k [a] -> Map.Map a [k]
invert dictOfLists =
  (Set.toList . Set.fromList . concat . Map.elems) dictOfLists
  |> map (\v -> (v, Map.keys $ Map.filter (\vals -> v `elem` vals) dictOfLists))
  |> listDictionary

listDictionary :: (Eq a, Ord a, Ord b) => [(a, [b])] -> Map.Map a [b]
listDictionary = (Map.fromAscListWith (++)) . sort
elementSet listDict = Set.fromList $ concat $ Map.elems listDict
keySet listDict = Set.fromList $ Map.keys listDict
keysNotInValues xs = Set.difference (keySet xs) (elementSet xs)

remove e xs = [x | x <- xs, e /= x]

toDictionary fileData =
  fileData
  |> map words
  |> (\steps -> (map (\xs -> (xs !! 1 !! 0, [xs !! 7 !! 0])) steps))
  |> listDictionary

nodeReady steps alreadyDone node =
  case Map.lookup node (invert steps) of
    Just xs -> (Set.fromList xs `Set.difference` Set.fromList alreadyDone) == Set.empty
    Nothing -> True

resolver :: Ord a => Map.Map a [a] -> [a] -> [a] -> [a]
resolver steps alreadyDone [] = reverse alreadyDone
resolver steps alreadyDone toDo@(_:_) =
  let nextNode = head $ filter (nodeReady steps alreadyDone) toDo
      remainingSteps = remove nextNode toDo
  in
  case Map.lookup nextNode steps of
    Just xs -> resolver steps (nextNode:alreadyDone) (sort $ xs ++ remainingSteps)
    Nothing -> resolver steps (nextNode:alreadyDone) remainingSteps

rootNodes = sort . Set.toList . keysNotInValues

partOne :: Map.Map Char [Char] -> [Char]
partOne steps =
  steps
  |> rootNodes
  |> resolver steps []

-- Worker Pool

type Task = Char
type Seconds = Int
type WorkItem = (Task, Seconds)
type Worker = Maybe WorkItem

tick :: [Worker] -> [Worker]
tick pool = map incrementTime pool
   where incrementTime worker =
           case worker of
             Just (work, time) -> Just (work, time + 1)
             Nothing -> Nothing

hasFreeWorkers pool = any ((==) Nothing) pool

giveNewTask :: [Worker] -> Task -> Maybe [Worker]
giveNewTask pool task =
  case hasFreeWorkers pool of
    True -> Just $ Just (task, 0):newWorkers
    False -> Nothing
  where newWorkers = onlyWorking ++ (take (length pool - 1) $ repeat Nothing)
        onlyWorking = filter ((/=) Nothing) pool

taskDone :: WorkItem -> Bool
taskDone (task, timeRan) = timeRan >= (ord task - 4)

getFinishedTask :: [Worker] -> Maybe (WorkItem, [Worker])
getFinishedTask pool =
  case any taskDone (catMaybes pool) of
    True -> Just (finishedTask, newPool)
    False -> Nothing
  where finishedTask = head $ filter taskDone (catMaybes pool)
        newPool = remove (Just finishedTask) pool ++ [Nothing]

timedResolver :: Map.Map Task [Task] -> [Worker] -> [Task] -> (Int, [WorkItem])
timedResolver mapping workerPool startingNodes = go [] startingNodes workerPool 0
   where go finished [] pool time =
            case all ((==) Nothing) pool of
              True -> (time, reverse finished)
              False ->
                case getFinishedTask pool of
                  Just (finishedTask, newPool) ->
                    case Map.lookup (fst finishedTask) mapping of
                      Just xs -> go (finishedTask:finished) (sort xs) newPool time
                      Nothing -> go (finishedTask:finished) [] newPool time
                  Nothing -> go finished [] (tick pool) (time + 1)
         go finished toDo@(_:_) pool time =
           case getFinishedTask pool of
             Just (finishedTask, newPool) ->
               case Map.lookup (fst finishedTask) mapping of
                 Just xs -> go (finishedTask:finished) (sort $ xs ++ toDo) newPool time
                 Nothing -> go (finishedTask:finished) toDo newPool time
             Nothing ->
               case Safe.headMay $ filter (nodeReady mapping (map fst finished)) toDo of
                 Nothing -> go finished toDo (tick pool) (time + 1)
                 Just nextTask ->
                   let remainingSteps = remove nextTask toDo
                   in
                   case giveNewTask pool nextTask of
                       Just workers -> go finished remainingSteps workers time
                       Nothing -> go finished toDo (tick pool) (time + 1)

-- nodeReady steps alreadyDone node =
--   case Map.lookup node (invert steps) of
--     Just xs -> (Set.fromList xs `Set.difference` Set.fromList alreadyDone) == Set.empty
--     Nothing -> True
--
-- resolver :: Ord a => Map.Map a [a] -> [a] -> [a] -> [a]
-- resolver steps alreadyDone [] = reverse alreadyDone
-- resolver steps alreadyDone toDo@(_:_) =
--   let nextNode = head $ filter (nodeReady steps alreadyDone) toDo
--       remainingSteps = remove nextNode toDo
--   in
--   case Map.lookup nextNode steps of
--     Just xs -> resolver steps (nextNode:alreadyDone) (sort $ xs ++ remainingSteps)
--     Nothing -> resolver steps (nextNode:alreadyDone) remainingSteps

partTwo steps =
  let pool = take 5 $ repeat Nothing in
  steps
  |> rootNodes
  |> timedResolver steps pool
  |> fst

daySevenMain :: IO ()
daySevenMain = do
  fileData <- getFile "inputs/DaySevenInput" >>= pure . toDictionary
  putStrLn $ show $ partOne fileData
  putStrLn $ show $ partTwo fileData

  pure ()
