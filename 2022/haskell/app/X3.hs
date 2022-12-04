import Data.List (elemIndex, nub)
import Data.Maybe (fromJust)
import Data.List.Split (chunksOf)


main :: IO ()
main = do
  input <- lines <$> readFile "../inputs/3"

  -- part one
  let priority c = fromJust (elemIndex c (['a'..'z'] ++ ['A'..'Z'])) + 1

      halve str =
        let half = length str `div` 2
        in (take half str, drop half str)

  print $ sum [priority c | (l, r) <- map halve input, c <- nub l, c `elem` r]

  -- part two
  print $ sum [priority z | [a, b, c] <- chunksOf 3 input, z <- nub a, z `elem` b && z `elem` c]
