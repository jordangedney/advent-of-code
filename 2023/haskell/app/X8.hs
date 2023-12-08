import Lude
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust)
import Data.Vector.Generic (forM_)

network :: Parser (String, (String, String))
network = do
  start <- some alphaNumChar <* " = ("
  left <- some alphaNumChar <* ", "
  right <- some alphaNumChar <* ")"
  pure (start, (left, right))

data Dir = L | R deriving (Show, Eq)

main :: IO ()
main = do
  let input = input2
  input <- lines <$> readFile "inputs/8"

  -- part one
  let directions = head input & map (\case 'R' -> R; _ -> L )
      parsed = map (parse network "") (drop 2 input) & rights
             & Map.fromList
      move L m i = Map.lookup i m & fromJust & fst
      move R m i = Map.lookup i m & fromJust & snd
      go (d:ds) m pos goal steps = 
        if pos == goal then steps
        else go ds m (move d m pos) goal (steps + 1)
  -- print $ go (cycle directions) parsed "AAA" "ZZZ" 0

  -- part two
  let starts = sort [k | k@[_, _, x] <- Map.keys parsed, x == 'A']
      takeUntilRepeat ys = tUR ys []
        where tUR (x:xs) seen = if x `elem` seen then reverse seen else tUR xs (x:seen)

      doMove m [] pos = [pos]
      doMove m (d:ds) pos = 
        let nPos = move d m pos
        in nPos : doMove m ds nPos
  --print $ takeUntilRepeat [1, 2, 3, 4, 2, 3, 4]
  -- print $ takeUntilRepeat (doMove parsed (cycle directions) "11A")
      seq str = doMove parsed (cycle directions) str & zip [1..] & filter (\(_, y) -> last y == 'Z')

  print $ starts
  let x = [take 10 $ seq s | s <- starts]
  mapM_ print x
  let y = map (map fst) x
  print y

  let z = [map (\(a, b) -> b - a) (zip xs (tail xs)) | xs <- y]
  print $ map last z & foldl1 lcm


input1 =
  [ "RL"
  , ""
  , "AAA = (BBB, CCC)"
  , "BBB = (DDD, EEE)"
  , "CCC = (ZZZ, GGG)"
  , "DDD = (DDD, DDD)"
  , "EEE = (EEE, EEE)"
  , "GGG = (GGG, GGG)"
  , "ZZZ = (ZZZ, ZZZ)" ]
input2 = [ "LR"
  , ""
  , "11A = (11B, XXX)"
  , "11B = (XXX, 11Z)"
  , "11Z = (11B, XXX)"
  , "22A = (22B, XXX)"
  , "22B = (22C, 22C)"
  , "22C = (22Z, 22Z)"
  , "22Z = (22B, 22B)"
  , "XXX = (XXX, XXX)" ]