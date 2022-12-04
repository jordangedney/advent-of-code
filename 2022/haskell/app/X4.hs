import Data.List.Split (splitOn)
import Data.Functor ((<&>))

main :: IO ()
main = do
  input :: [((Int, Int), (Int, Int))]
    <- readFile "../inputs/4"
    <&> lines
    <&> map (splitOn ",")
    <&> map (map (splitOn "-"))
    <&> map (\[[x, y], [a, b]] -> ((read x, read y), (read a, read b)))

  -- part one
  let contains (x, y) (a, b) = x <= a && y >= b

  print $ length [z | z@(x, y) <- input, contains x y || contains y x]

  -- part two
  print $ length [z | z@((x, y), (a, b)) <- input, any (`elem` [a..b]) [x..y]]
