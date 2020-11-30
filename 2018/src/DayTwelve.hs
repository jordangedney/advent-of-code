{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BangPatterns #-}

module DayTwelve where

import Util ((|>), apply)

import Brick (Widget, withBorderStyle, str, (<+>))
import BrickHelper (stepBasedUI)
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Border.Style as BS
import qualified Brick.Widgets.Center as C
import qualified Data.Map as Map
import Data.Map ((!))
import qualified Data.Set as Set

rules =
  [ "##.#. => ."
  , "##.## => ."
  , "#..## => ."
  , "#.#.# => ."
  , "..#.. => #"
  , "#.##. => ."
  , "##... => #"
  , ".#..# => ."
  , "#.### => ."
  , "..... => ."
  , "...#. => #"
  , "#..#. => #"
  , "###.. => #"
  , ".#... => #"
  , "###.# => #"
  , "####. => ."
  , ".##.# => #"
  , "#.#.. => #"
  , ".###. => #"
  , ".#.## => ."
  , "##### => #"
  , "....# => ."
  , ".#### => ."
  , ".##.. => #"
  , "##..# => ."
  , "#...# => ."
  , "..### => #"
  , "...## => ."
  , "#.... => ."
  , "..##. => ."
  , ".#.#. => #"
  , "..#.# => #"
  ]

data BTree = Leaf | Branch BTree BTree deriving (Show, Eq)

hah [x1, x2, x3, x4, x5] = (x1, x2, x3, x4, x5)
type Pots = (Bool, Bool, Bool, Bool, Bool)
allBTreesOfSizeFive :: Map.Map Pots BTree
allBTreesOfSizeFive =
  Map.fromList $ map (\s -> (hah s, mkBTree' s)) allPotentialPotStates
  where mkBTree' [] = Branch Leaf Leaf
        mkBTree' (True:xs) = Branch Leaf (mkBTree' xs)
        mkBTree' (False:xs) = Branch (mkBTree' xs) Leaf


        allPotentialPotStates =
          [[p1, p2, p3, p4, p5] | p1 <- opt, p2 <- opt, p3 <- opt, p4 <- opt, p5 <- opt]
          where opt = [True, False]

mergeBTree l Leaf = l
mergeBTree Leaf r = r
mergeBTree (Branch l1 l2)  (Branch r1 r2) =
  Branch (mergeBTree l1 r1) (mergeBTree l2 r2)

Leaf `inBTree` _ = True
_ `inBTree` Leaf = False
(Branch l1 l2) `inBTree` (Branch r1 r2) =
  and [l1 `inBTree` r1, l2 `inBTree` r2]

parsedBTree =
  rules
  |> map words
  |> filter (\x -> (last x) == "#")
  |> map (\x -> x !! 0)
  |> map plantStrToBTree
  |> foldr1 mergeBTree

plantStrToBTree x = allBTreesOfSizeFive ! (hah (map plantAlive x))

plantAlive '#' = True
plantAlive _ = False

parseRule rule = map plantAlive (words rule !! 0)
parseRules = map parseRule . filter ((=='#') . last)

initPlants = "##..##....#.#.####........##.#.#####.##..#.#..#.#...##.#####.###.##...#....##....#..###.#...#.#.#.#"

data State = State
  { counter :: Int
  , plants :: Set.Set Int
  , leftmost :: Int
  , rightmost :: Int
  , loopHit :: Bool
  }

indexToBTree index allPots = allBTreesOfSizeFive ! pots
  where m p = Set.member p allPots
        pots = (m (index - 2), m (index - 1), m index, m (index + 1), m (index + 2))

initState = State 0 potSet 0 ((length initPlants) - 1) False
  where onlyPotsWithPlants = filter (\(c, i) -> (c == '#')) (zip initPlants [0..])
        potSet = Set.fromAscList $ map (\(c, i) -> i) onlyPotsWithPlants

growNewPlants State{..} =
  Set.fromList $ filter (shouldGrow plants) $ [leftmost - 4 .. rightmost + 4]
--   foldr updatePlants plants [leftmost - 4 .. rightmost + 4]
--
  where shouldGrow allPots p = (indexToBTree p allPots) `inBTree` parsedBTree
--
--         updatePlants index newPlants =
--           if shouldGrow plants index
--           then Set.insert index newPlants
--           else Set.delete index newPlants

updater s@State{..} =
  if not loopHit
  then s { counter = (counter + 1)
         , plants = plants'
         , leftmost = leftmost'
         , rightmost = rightmost'
         , loopHit = plants' == incrementedPlants
         }
  else s
  where plants' = growNewPlants s
        leftmost' = head $ Set.toAscList plants'
        rightmost' = head $ Set.toDescList plants'
        incrementedPlants = Set.map (+1) plants


-- countPlants = sum . (map index) . (filter hasPlant)
countPlants allPots = sum $ Set.toAscList allPots

states = iterate updater initState

-- partOne = brickUI initState updater
partOne = printfUI (take 21 states)
-- partTwo = printfUI (take 1000 states)

partTwoHacking = go initState
  where target = 50000000000

        go s@State{..} =
          if loopHit
          then s { plants = Set.map (+ (target - counter)) plants}
          else go (updater s)

partTwo = printfUI [partTwoHacking]
-- partTwo = printfUI [apply 50 updater initState]

--  dayTwelveMain = partTwo
-- dayTwelveMain = putStrLn $  show $ updater initState
dayTwelveMain = partTwo

-- Faffy UI stuff

brickUI state updateFn= stepBasedUI 1000000 state updateFn drawUI

printfUI = mapM_ (putStrLn . visualizePlants)

visualizePlants State{..} =
  (show . countPlants) plants ++ " " ++ " "
  ++ (show $ length plants) ++ " " ++ (show plants) ++ " "

drawUI :: State -> [Widget ()]
drawUI s@State{..} =
  [withBorderStyle BS.unicode $
   B.borderWithLabel (str "Plants") $
   -- (C.center (str (show $ counter s)) <+> B.vBorder <+> C.center (str "Right"))]
   (C.center (str $ visualizePlants s))
  ]

printBlank n = putStr $ take n $ repeat ' '
printBTree t = putStrLn "" >> printBTree' 0 t >> putStrLn "" >> putStrLn ""
printBTree' n Leaf = printBlank n >> putStr "Leaf"
printBTree' n (Branch l r) =
  printBlank n >> putStrLn "Left:" >> printBTree' (n + 2) l >>
  putStrLn "" >> printBlank n >> putStrLn "Right:" >> printBTree' (n + 2) r
