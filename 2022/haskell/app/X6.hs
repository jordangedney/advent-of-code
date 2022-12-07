import Data.List (nub)
import Data.Function ((&))

main :: IO ()
main = do
  input <- head <$> lines <$> readFile "../inputs/6"

  let get amt i []  = []
      get amt i f@(_:xs) =
        (i , take amt f) : get amt (i + 1) xs

  print $ dropWhile (\(y, x) -> (not (length(nub x) == length x))) (get 4 4 input) & head
  print $ dropWhile (\(y, x) -> (not (length(nub x) == length x))) (get 14 14 input) & head
