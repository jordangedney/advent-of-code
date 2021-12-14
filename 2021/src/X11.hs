module X11 (main) where

import Lude
import qualified Data.Map as M

data Octopus = Charging Int | Flashing Int deriving (Eq, Show)

parse :: [String] -> Map Point Octopus
parse = map Charging . mkGrid

reset  = \case { Flashing _ -> Charging 0; y -> y; }
charge = \case { Charging x -> Charging (x + 1); y -> y; }

stepOne :: Map Point Octopus -> Map Point Octopus
stepOne = map charge

stepTwo' :: (Int, Map Point Octopus) -> (Int, Map Point Octopus)
stepTwo' (f, g) =
  let ready = M.keys (M.filter (\case { Charging x -> x > 9; _ -> False }) g)
      toInc = map adjacents ready |> concat
      flash  = \case { Charging x -> Flashing x; y -> y; }
  in mApply charge toInc g |> mApply flash ready |> (\g' -> (length ready + f, g'))

stepTwo :: (Int, Map Point Octopus) -> (Int, Map Point Octopus)
stepTwo = last . doFixed stepTwo'

stepThree :: Map Point Octopus -> Map Point Octopus
stepThree = map reset

lightShow (f, g) = stepTwo (f, stepOne g) |> (\(f', g') -> (f', stepThree g'))

part1 g = (iterate lightShow (0, g)) !! 100

part2 g =
  let nOct = length (M.keys g)
  in zip [0..] (iterate (\(_, g') -> lightShow (0, g')) (0, g))
     |> dropWhile (\(_, (x, _)) -> x /= nOct)
     |> head

main :: IO ()
main = readFile "inputs/11" <&> lines >>> parse >>> part2 >>= print
