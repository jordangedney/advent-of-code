{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BangPatterns #-}

module DayThirteen where

import Util ((|>), getFile)
import qualified Data.Map as Map
import Data.Map ((!))
import qualified Data.Set as Set
import Control.Monad (liftM)
import Prelude hiding (Left, Right)
import Debug.Trace

-- /->-\
-- |   |  /----\
-- | /-+--+-\  |
-- | | |  | v  |
-- \-+-/  \-+--/
--   \------/

testTrackOne =
  [ "/->-\\"
  , "|   |  /----\\"
  , "| /-+--+-\\  |"
  , "| | |  | v  |"
  , "\\-+-/  \\-+--/"
  , "  \\------/"
  ]

-- />-<\
-- |   |
-- | /<+-\
-- | | | v
-- \>+</ |
--   |   ^
--   \<->/

testTrackTwo =
  [ "/>-<\\"
  , "|   |"
  , "| /<+-\\"
  , "| | | v"
  , "\\>+</ |"
  , "  |   ^"
  , "  \\<->/"
  ]

data TrackConnection
 = DownRight     -- /
  | DownLeft     -- \
  | LeftRight    -- -
  | UpDown       -- |
  | Intersection -- +
  | UpLeft       -- /
  | UpRight      -- \
  | NoConnection
  deriving (Eq, Show)

data Direction
  = Left | Right | Up | Down
  deriving (Eq, Show)

data IntersectionDirection
  = GoLeft | GoStraight | GoRight
  deriving (Eq, Show)

type CurrentDirection = Direction
type PreviouslyTakenDirection = IntersectionDirection
type Cart = (CurrentDirection, PreviouslyTakenDirection)

data State = State
  { tracks :: Map.Map Coord TrackConnection
  , carts :: Map.Map Coord Cart
  , traversalOrder :: [Coord]
  } deriving (Eq, Show)

type Coord = (Int, Int)

left :: Coord -> Coord
left (x, y) = (x - 1, y)

right :: Coord -> Coord
right (x, y) = (x + 1, y)

down :: Coord -> Coord
down (x, y) = (x, y + 1)

up :: Coord -> Coord
up (x, y) = (x, y - 1)

neighbors :: Coord -> [Coord]
neighbors c = [left c, right c, down c, up c]

parseUnclassifiedTrack :: [String] -> State
parseUnclassifiedTrack trackTokens =
  let withCoordinatesAdded :: [(Coord, Char)]
      withCoordinatesAdded =
        map (zip [0..]) trackTokens
        |> zip [0..]
        |> map (\(y, line) ->
                  map (\(x, t) -> ((x, y), t)) line)
        |> concat

      traversalOrder :: [Coord]
      traversalOrder =
        withCoordinatesAdded
        |> filter ((not  . (== ' ')) . snd)
        |> map fst

      coordsToUnclassifiedTrack :: Map.Map Coord Char
      coordsToUnclassifiedTrack =
        Map.fromList withCoordinatesAdded
        |> flip Map.restrictKeys (Set.fromList traversalOrder)

      classifyMinecart :: Char -> TrackConnection
      classifyMinecart unclassifiedTrack =
        -- This would normally be tricky, however the given input was kind
        case unclassifiedTrack of
          '<' -> LeftRight
          '>' -> LeftRight
          'v' -> UpDown
          '^' -> UpDown
          _   -> error $ "Error in minecart classification" ++ show unclassifiedTrack

      classifyCorner :: Coord -> Char -> TrackConnection
      classifyCorner coord unclassifiedTrack =
        let getTrack :: Coord -> TrackConnection
            getTrack =
              flip (Map.findWithDefault NoConnection) coordsToClassifiedTrack

            err@[left, right, down, up] = map getTrack $ neighbors coord
        in case unclassifiedTrack of
          '\\' -> case [left, right, down, up] of
            [LeftRight,    _,            UpDown,       _]            -> DownLeft
            [Intersection, _,            UpDown,       _]            -> DownLeft
            [LeftRight,    _,            Intersection, _]            -> DownLeft
            [Intersection, _,            Intersection, _]            -> DownLeft
            [_,            LeftRight,    _,            UpDown]       -> UpRight
            [_,            Intersection, _,            UpDown]       -> UpRight
            [_,            LeftRight,    _,            Intersection] -> UpRight
            [_,            Intersection, _,            Intersection] -> UpRight
            _   -> error $ "Error in (dl/ur) classification " ++ show err

          '/'  -> case [left, right, down, up] of
            [_,            LeftRight,    UpDown,       _]            -> DownRight
            [_,            Intersection, UpDown,       _]            -> DownRight
            [_,            LeftRight,    Intersection, _]            -> DownRight
            [_,            Intersection, Intersection, _]            -> DownRight
            [LeftRight,    _,            _,            UpDown]       -> UpLeft
            [Intersection, _,            _,            UpDown]       -> UpLeft
            [LeftRight,    _,            _,            Intersection] -> UpLeft
            [Intersection, _,            _,            Intersection] -> UpLeft
            _   -> error $ "Error in (dr/ul) classification " ++ show err

      classify :: Coord -> Char -> TrackConnection
      classify coord unclassifiedTrack =
        case unclassifiedTrack of
          '-'  -> LeftRight
          '|'  -> UpDown
          '+'  -> Intersection
          '\\' -> classifyCorner coord unclassifiedTrack
          '/'  -> classifyCorner coord unclassifiedTrack
          _    -> classifyMinecart unclassifiedTrack

      coordsToClassifiedTrack :: Map.Map Coord TrackConnection
      coordsToClassifiedTrack = Map.mapWithKey classify coordsToUnclassifiedTrack

      carts :: Map.Map Coord Cart
      carts =
        let player :: Char -> Bool
            player t = or [t == '<', t == '>', t == 'v', t == '^']

            playerDirection :: Char -> Cart
            playerDirection t = case t of
              '<' -> (Left, GoRight)
              '>' -> (Right, GoRight)
              'v' -> (Down, GoRight)
              '^' -> (Up, GoRight)
              _   -> error $ "Error during direction classification " ++ show t
        in Map.map playerDirection $ Map.filter player coordsToUnclassifiedTrack
  in State coordsToClassifiedTrack carts traversalOrder

initState :: IO State
initState = getFile "inputs/DayThirteenInput" >>= return . parseUnclassifiedTrack

update :: State -> ([Coord], State)
update s@State{..} =
  let cartTraversalOrder :: [Coord]
      cartTraversalOrder = filter (flip Map.member carts) traversalOrder

      nextPosition :: Coord -> (Coord, Cart)
      nextPosition cartCoords =
        let (curDir, prevIntersection) = carts ! cartCoords
            curTrack = tracks ! cartCoords

            goDirection' ::
              PreviouslyTakenDirection -> Coord -> Direction -> (Coord, Cart)
            goDirection' p c Left = (left c,  (Left, p))
            goDirection' p c Right = (right c, (Right, p))
            goDirection' p c Up = (up c, (Up, p))
            goDirection' p c Down = (down c, (Down, p))

            nextDirection' :: IntersectionDirection -> IntersectionDirection
            nextDirection' GoLeft     = GoStraight
            nextDirection' GoStraight = GoRight
            nextDirection' GoRight    = GoLeft

            goDirection :: Direction -> (Coord, Cart)
            goDirection = goDirection' prevIntersection cartCoords

            nextDirection :: IntersectionDirection
            nextDirection = nextDirection' prevIntersection

            goIntersection :: Direction -> (Coord, Cart)
            goIntersection = goDirection' nextDirection cartCoords

            pickDir :: TrackConnection -> Direction -> Direction
            pickDir DownRight Left = Down
            pickDir DownRight Up   = Right

            pickDir DownLeft Right = Down
            pickDir DownLeft Up    = Left

            pickDir LeftRight Left  = Left
            pickDir LeftRight Right = Right

            pickDir UpDown Up   = Up
            pickDir UpDown Down = Down

            pickDir Intersection dir = pickIntersection nextDirection dir

            pickDir UpLeft Right = Up
            pickDir UpLeft Down  = Left

            pickDir UpRight Left = Up
            pickDir UpRight Down = Right

            pickIntersection :: IntersectionDirection -> Direction -> Direction
            pickIntersection GoLeft Left  = Down
            pickIntersection GoLeft Right = Up
            pickIntersection GoLeft Up    = Left
            pickIntersection GoLeft Down  = Right

            pickIntersection GoStraight Left  = Left
            pickIntersection GoStraight Right = Right
            pickIntersection GoStraight Up    = Up
            pickIntersection GoStraight Down  = Down

            pickIntersection GoRight Left  = Up
            pickIntersection GoRight Right = Down
            pickIntersection GoRight Up    = Right
            pickIntersection GoRight Down  = Left

            updateCart :: TrackConnection -> (Direction -> (Coord, Cart))
            updateCart Intersection = goIntersection
            updateCart _            = goDirection

        in updateCart curTrack $ pickDir curTrack curDir

      newCarts :: [(Coord, Cart)]
      newCarts = map nextPosition cartTraversalOrder

      collisions :: [Coord]
      collisions =
        let newCartCoordinates :: [Coord]
            newCartCoordinates = map fst newCarts

            accumulator :: (Set.Set Coord, [Coord])
            accumulator = (Set.empty, [])

            go :: Coord -> (Set.Set Coord, [Coord]) -> (Set.Set Coord, [Coord])
            go coord (seen, dups) =
              if Set.member coord seen
              then (seen, dups ++ [coord])
              else (Set.insert coord seen, dups)
        in snd $ foldr go accumulator newCartCoordinates

  in (collisions, s { carts = Map.fromList newCarts })

predictFirstCrash :: State -> Coord
predictFirstCrash s =
  let (collisions, newState) = update s
  in if not $ null collisions
     then head collisions
     else predictFirstCrash newState

predictFirstCrash' s =
  let (collisions, newState) = update s
  in if not $ null collisions
     then collisions
     else predictFirstCrash' newState

partOne :: State -> Coord
partOne = predictFirstCrash

-- getLastManStanding :: State -> Coord
getLastManStanding s =
  let (collisions, newState) = update s
      cartsWithCollisionsRemoved =
        foldr (\coord carts -> Map.delete coord carts) (carts newState) collisions
      newStateWithoutCollisions = newState {carts = cartsWithCollisionsRemoved}
  in if (Map.size (carts s) > 1)
     then getLastManStanding newStateWithoutCollisions
     else head $ Map.keys (carts s)

getLastManStanding' s =
  let (collisions, newState) = update s
      cartsWithCollisionsRemoved =
        foldr (\coord carts -> Map.delete coord carts) (carts newState) collisions
      newStateWithoutCollisions = newState {carts = cartsWithCollisionsRemoved}
  in if (Map.size (carts s) > 1)
     then trace (show (carts newStateWithoutCollisions) ++ "\n")  $ getLastManStanding' newStateWithoutCollisions
     else head $ Map.keys (carts s)

-- partTwo :: State -> Coord
partTwo = getLastManStanding

dayThirteenMain = do
  state <- initState
  putStrLn . show $ partOne state
  putStrLn . show $ partTwo state

  pure ()
