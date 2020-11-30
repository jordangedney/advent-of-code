module Three where

import Util (splitOn, Coord, manhattan)
import qualified Data.Map.Strict as Map

intersections :: [[Coord]] -> Map.Map Coord Int
intersections = foldr1 (Map.intersectionWith (+)) . map (Map.fromList . flip zip [1..])

partOne :: [[Coord]] -> Int
partOne = minimum . map (manhattan (0, 0)) . Map.keys . intersections

partTwo :: [[Coord]] -> Int
partTwo = minimum . Map.elems . intersections

progMain :: IO ()
progMain = do
  wires <- map parse . lines <$> readFile "inputs/three"
  print $ partOne wires
  print $ partTwo wires

parse :: String -> [Coord]
parse = followTheWire . map mkGetNextPointFn . splitOn ","

mkGetNextPointFn :: String -> (Coord -> Coord)
mkGetNextPointFn [] = error "Unexpected end of input while parsing"
mkGetNextPointFn (c:cs) =
  let move x y (a, b) = (a + x, b + y)
      amount = read cs
  in case c of
     'L' -> move (- amount) 0
     'R' -> move amount 0
     'D' -> move 0 (- amount)
     'U' -> move 0 amount
     err -> error $ "Unexpected character " ++ [err] ++ " while parsing"

followTheWire :: [Coord -> Coord] -> [Coord]
followTheWire = tail . reverse . foldl calcLine [(0, 0)]
  where calcLine [] _ = error "Unexpected end of arguments"
        calcLine ((x, y):wire) getNextPoint =
          let (a, b) = getNextPoint (x, y)
              line = [(c, d) | c <- [min x a .. max x a], d <- [min y b .. max y b]]
          in if head line == (a, b) then line ++ wire else reverse line ++ wire
