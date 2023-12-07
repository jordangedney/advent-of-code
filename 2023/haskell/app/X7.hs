import Lude
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust)
import Data.List (elemIndex)
import Data.Sort (sortBy)

hand :: Parser (String, Int)
hand = (,) <$> (some alphaNumChar <* ws) <*> entry

data Hand = Five | Four | Full | Three | Two | One | High deriving (Show, Eq, Ord)

main :: IO ()
main = do
  let input =
        [ "32T3K 765"
        , "T55J5 684"
        , "KK677 28"
        , "KTJJT 220"
        , "QQQJA 483" ]
  input <- lines <$> readFile "inputs/7"

  -- part one
  let parsed = map (parse hand "") input & rights

      handType xs
        | 5 `elem` xs                = Five
        | 4 `elem` xs                = Four
        | 3 `elem` xs && 2 `elem` xs = Full
        | 3 `elem` xs                = Three
        | numOf 2 xs == 2            = Two
        | numOf 2 xs == 1            = One
        | otherwise                  = High

      getHandType1 xs = handType (Map.elems $ frequencies xs)

      compareHands' getHandType cards a b
        | getHandType a < getHandType b = GT
        | getHandType a > getHandType b = LT
        | otherwise =
           let cardScore c = elemIndex c (reverse cards) & fromJust & (+ 1)
               scored = zip (map cardScore a) (map cardScore b) & dropWhile (uncurry (==))
          in case scored of
            ((a, b):_)     -> compare a b
            _              -> EQ

      solveBy comparison = 
        sortBy (\(h1, _) (h2, _) -> comparison h1 h2) parsed 
        & zip [1..] 
        & map (\(r, (_, s)) -> r * s) 
        & sum

  print $ solveBy (compareHands' getHandType1 "AKQJT98765432")

  -- part two
  let getHandType2 xs =
        let jokerCount = numOf 'J' xs
            noJoke = filter (/= 'J') xs
        in case jokerCount of
             0 -> getHandType1 xs
             1 -> case getHandType1 noJoke of
               Five   -> Five
               Four   -> Five
               Full   -> Four
               Three  -> Four
               Two    -> Full
               One    -> Three
               High   -> One
             2 -> case getHandType1 noJoke of
               Five   -> Five
               Four   -> Five
               Full   -> Five
               Three  -> Five
               Two    -> Four
               One    -> Four
               High   -> Three
             3 -> case getHandType1 noJoke of
               Five   -> Five
               Four   -> Five
               Full   -> Five
               Three  -> Five
               Two    -> Five
               One    -> Five
               High   -> Four
             _ -> Five

  print $ solveBy (compareHands' getHandType2 "AKQT98765432J")
