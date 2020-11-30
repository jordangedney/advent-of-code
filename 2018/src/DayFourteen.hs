{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BangPatterns #-}

module DayFourteen where

import Util ((|>), digits, enumerate)
import qualified Data.Sequence as S
import Control.Monad (join)
import Data.Foldable (toList)
import Data.Maybe (fromMaybe)


data QueySeq a = QueySeq {maxSize :: Int, vals :: S.Seq a} deriving (Show, Eq)
q@QueySeq{..} `append` s = q {vals = takeR maxSize $ vals S.>< s}
(QueySeq _ v) `has` s = any (== s) $ combinations 6 v

takeR :: Int -> S.Seq a -> S.Seq a
takeR n l = go (S.drop n l) l
  where
    go S.Empty r = r
    go (_ S.:<| xs) (_ S.:<| ys) = go xs ys

combinations size seq
  | S.length seq < size = [seq]
  | otherwise = (S.take size seq) : combinations size (S.drop 1 seq)

-- There are two elfs making coco, lets name them jack and jill
data State = State
  { jack :: Int
  , jill :: Int
  , cocoScores :: S.Seq Int
  , recentScores :: QueySeq Int
  } deriving (Show, Eq)

initState :: State
initState = State 0 1 (S.fromList [3, 7]) (QueySeq 8 S.empty)

toStr State{..} =
  toList cocoScores
  |> enumerate
  |> map (\(i, v) ->
            case () of -- Hacky, I like it.
              _ | i == jack && i == jill -> "([" ++ show v ++ "]) "
                | i == jack  -> "(" ++ show v ++ ") "
                | i == jill  -> "[" ++ show v ++ "] "
                | otherwise -> show v ++ " ")
  |> join

-- newRecipes State{..} =
--   case (cocoScores S.!? jack, cocoScores S.!? jill) of
--     (Just jk, Just jl) -> case digits (jk + jl) of
--                            [c] -> cocoScores S.|> c
--                            [c, cc] -> cocoScores S.|> c S.|> cc

nextIndex :: S.Seq Int -> Int -> Int
nextIndex scores elf =
  case scores S.!? elf of
    Just s -> s
             |> (+ 1)
             |> (+ elf)
             |> (`rem` (S.length scores))

newRecipes State{..} =
  map (fromMaybe (error "Fk")) [cocoScores S.!? jack, cocoScores S.!? jill]
  |> S.fromList . digits . sum

updateState s@State{..} = State (nI jack) (nI jill) nS rS
  where nR = newRecipes s
        nS = cocoScores S.>< nR
        nI = nextIndex nS
        rS = recentScores `append` nR

visualIterate :: Int -> State -> IO ()
visualIterate 0 s = do pure ()
visualIterate n s = do
  putStrLn $ toStr s
  visualIterate (n - 1) (updateState s)

partOne x =
  (iterate updateState initState)
  |> takeWhile (\s -> (S.length . cocoScores) s < (x + 15))
  |> last
  |> toList . cocoScores
  |> drop x
  |> take 10
  |> join . (map show)

res = S.fromList [7, 6, 8, 0, 7, 1]

partTwo =
  (iterate updateState initState)
  |> takeWhile (\s -> (not (recentScores s `has` res)))
  |> toList . cocoScores . last
  |> (+ (-5)) . length

dayFourteenMain :: IO ()
dayFourteenMain = do
  -- state <- initState
  let state = initState
  -- putStrLn . show $ partOne 768071
  putStrLn . show $ partTwo
  -- putStrLn . show $ partOne 2018
  -- putStrLn . show $ partTwo state
  pure ()
