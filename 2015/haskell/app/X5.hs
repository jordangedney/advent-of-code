import Lude
import Data.List (isInfixOf)

main :: IO ()
main = do 
  input <- lines <$> readFile "inputs/5"

  -- part one
  let vowel = (`elem` "aeiou")
      threeVowels = filter vowel |> length |> (>= 3)
      hasDouble (x:y:xs) = (x == y) || hasDouble (y:xs)
      hasDouble _ = False
      noBanned xs = not (any (`isInfixOf` xs) ["ab", "cd", "pq", "xy"])
      niceString s = all ($ s) [threeVowels, hasDouble, noBanned]
  print $ filter niceString input & length

  -- part two
  let hasLaterDouble (x:y:xs) = [x, y] `isInfixOf` xs || hasLaterDouble (y:xs)
      hasLaterDouble _ = False
      hasSymPair (x:y:z:xs) = (x == z) || hasSymPair (y:z:xs)
      hasSymPair _ = False
      twiceNice s = all ($ s) [hasLaterDouble, hasSymPair]
  print $ filter twiceNice input & length