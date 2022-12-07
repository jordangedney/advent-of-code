import Data.List.Split (splitOn)
import Data.Functor ((<&>))
import Data.Function ((&))
import Text.Read (readMaybe)
import Data.Maybe (catMaybes, fromJust)
import Data.List (transpose)


readInt :: String -> Maybe Int
readInt = readMaybe

parseInstructions xs =
  map words xs
  & map (map readInt)
  & map catMaybes
  & map (\[x, y, z] -> (x, y - 1, z - 1))

rotate :: [String] -> [String]
rotate = reverse . transpose . map reverse

replaceAt n val xs =
  let (x,_:ys) = splitAt n xs
  in x ++ val : ys

-- This is batshit crazy; I dunno why I'm doing it this way
parseCrates xs =
  let numCol = last xs & words & last & readInt & fromJust
      padEnd x = x ++ (take ((numCol * 4) - (length x)) (repeat ' '))
  in init xs
     & map padEnd
     & map tail
     & map (\x -> [x !! (i * 4) | i <- [0..numCol - 1]])
     & rotate
     & map (concat . words)

-- doIt :: [String] -> [(Int, Int, Int)] -> [String]
main :: IO ()
main = do
  input
    <- readFile "../inputs/5"
    <&> splitOn "\n\n"
    <&> map lines
    <&> (\xs -> (parseCrates (head xs), parseInstructions (last xs)))

  let doIt (crates, []) = crates
      doIt (crates, ((amt, from, to):xs)) =
        let toMove = (crates !! from) & take amt & reverse
            moved = toMove ++ (crates !! to)
        in doIt ((crates & replaceAt from (crates !! from & drop amt) & replaceAt to moved), xs)

  print $ doIt input & map head

  let doItAgain (crates, []) = crates
      doItAgain (crates, ((amt, from, to):xs)) =
        let toMove = (crates !! from) & take amt
            moved = toMove ++ (crates !! to)
        in doItAgain ((crates & replaceAt from (crates !! from & drop amt) & replaceAt to moved), xs)
  print $ doItAgain input & map head

  -- print $ doIt (head input) (last input)
  -- part one
  -- let contains (x, y) (a, b) = x <= a && y >= b

  -- print $ length [z | z@(x, y) <- input, contains x y || contains y x]

  -- -- part two
  -- print $ length [z | z@((x, y), (a, b)) <- input, any (`elem` [a..b]) [x..y]]
