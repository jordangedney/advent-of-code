import Lude
import qualified Data.Map.Strict as Map
import Data.Char (isDigit)
import Data.Function (on)

groupWith :: (a -> a -> Bool) -> [a] -> [[a]]
groupWith f (y:ys) = reverse . map reverse $ go ys [[y]]
  where go (x:xs) (a:acc) = if f x (head a)
                            then go xs ((x:a):acc)
                            else go xs ([x]:a:acc)
        go _ acc = acc

main :: IO ()
main = do
  let input =
        [ "467..114.."
        , "...*......"
        , "..35..633."
        , "......#..."
        , "617*......"
        , ".....+.58."
        , "..592....."
        , "......755."
        , "...$.*...."
        , ".664.598.." ]
  input <- lines <$> readFile "inputs/3"

  -- part one
  let parsed = Map.fromList
        [((x, y), c) | (x, i) <- zip [0..length input] input,
                       (y, c) <- zip [0..] i]

      right (x, y) = (x, y + 1)
      left  (x, y) = (x, y - 1)
      up    (x, y) = (x - 1, y)
      down  (x, y) = (x + 1, y)
      isNext x y = x == right y

      numbers = Map.filter isDigit parsed
              & Map.toAscList
              & groupWith (isNext `on` fst)

      adjacents :: (Int, Int) -> [(Int, Int)]
      adjacents x = [f x | f <- [right, left, up, down,
                                 right . up, right . down, left . up, left . down]]

      isSymbol c = (c /= '.') && not (isDigit c)
      isSymbolAdjacent x = any isSymbol [Map.findWithDefault '.' y parsed | y <- adjacents x]

      withAdjacentSymbols = [n | n <- numbers, any (isSymbolAdjacent . fst) n]
      partNumbers = mapMaybe (readInt . map snd) withAdjacentSymbols

  print $ sum partNumbers 

  -- part two
  let gears = Map.filter (== '*') parsed & Map.toAscList & map fst
      adjacentPartNumbers x = 
        [map snd ps | a <- adjacents x, ps <- withAdjacentSymbols, p <- ps, fst p == a]
        & nub
        & mapMaybe readInt

  print $  map adjacentPartNumbers gears
        & filter ((== 2) . length) 
        & map product
        & sum
