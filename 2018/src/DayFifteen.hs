{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BangPatterns #-}

module DayFourteen where

import Util ((|>))
import qualified Data.Vector as V
import Data.Sort (sort)
import Data.List (group)


puzzleInput =
  [ "################################"
  , "##############.#################"
  , "##########G##....###############"
  , "#########.....G.################"
  , "#########...........############"
  , "#########...........############"
  , "##########.....G...#############"
  , "###########.........############"
  , "########.#.#..#..G....##########"
  , "#######..........G......########"
  , "##..GG..................###.####"
  , "##G..........................###"
  , "####G.G.....G.#####...E.#.G..###"
  , "#....##......#######........####"
  , "#.GG.#####.G#########.......####"
  , "###..####...#########..E...#####"
  , "#...####....#########........###"
  , "#.G.###.....#########....E....##"
  , "#..####...G.#########E.....E..##"
  , "#..###G......#######E.........##"
  , "#..##.........#####..........###"
  , "#......................#..E....#"
  , "##...G........G.......#...E...##"
  , "##............#..........#..####"
  , "###.....#...#.##..#......#######"
  , "#####.###...#######...#..#######"
  , "#########...E######....#########"
  , "###########...######.###########"
  , "############..#####..###########"
  , "#############.E..##.############"
  , "################.#..############"
  , "################################"
  ]

example1 =
  [ "#######"
  , "#.G...#"
  , "#...EG#"
  , "#.#.#G#"
  , "#..G#E#"
  , "#.....#"
  , "#######"
  ]

-- printBoard (V.Vector P

type Health = Int
data Piece = Wall | Empty | Goblin Health | Elf Health

classify '#' = Wall
classify 'E' = Elf 200
classify 'G' = Goblin 200
classify _ = Empty

-- I'm feeling lazy, so I'll just use a single int as the index
data Width = Int
data Game = Game {board :: (V.Vector Piece, Width) }


ensure :: (a -> Bool) -> a -> Maybe a
ensure p a = if p a then pure a else Nothing

up l x = ensure (>= 0) (x - l)
right l x = ensure ((/= 0) . (`mod` l)) (x + 1)
down l last x = ensure (<= last) (x + l)
left l x = ensure ((/= 0) . (`mod` l) . (+1)) (x - 1)

parser :: [String] -> (V.Vector Piece, Int)
parser xs = ((V.fromList $ concatMap (map classify) xs), (length $ head xs))

dayFifteenMain :: IO ()
dayFifteenMain = do
  -- let state = initState
  pure ()


array = [10, 20, 20, 10, 10, 30, 50, 10, 20]

f = sort array |> group |> map length |> map (`div` 2) |> sum
