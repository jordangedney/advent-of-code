import qualified Data.Map as M
import Data.Map (Map)
import Data.Function((&))
import Data.Functor((<&>))

-- Grid Code: ------------------------------------------------------------------
type Point = (Int, Int)

type Grid = Map Point Int

mkGrid :: [String] -> Grid
mkGrid xs = map (map (read . (:[]))) xs
  & zip [0..]
  & map (\(a, ys) -> zip [0..] ys & map (\(b, v) -> ((a, b), v)))
  & concat
  & M.fromList
--------------------------------------------------------------------------------

main :: IO ()
main = do
  input <- readFile "../inputs/8.test" <&> lines <&> mkGrid

  print $ input













-- has diagonals
adjacents :: Point -> [Point]
adjacents (a, b) = [(x, y) | x <- [a-1..a+1], y <- [b-1..b+1], (a, b) /= (x, y)]

neighbors :: Point -> [Point]
neighbors (x, y) = [(x, y - 1), (x + 1, y), (x, y + 1), (x - 1, y)]

-- If your grid is a topleft to bottomright style:
positivePoints :: [Point] -> [Point]
positivePoints xs = [a | a@(x, y) <- xs, x >= 0, y >= 0]

mApply :: (Ord a) => (b -> b) -> [a] -> Map a b -> Map a b
mApply fn xs g = foldr (\p m -> M.adjust fn p m) g xs
