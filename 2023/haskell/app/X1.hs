import Lude
import Data.Char (isAlpha)

type Parser = Parsec Void String

wordMap = \case
  "one"   -> "1"
  "two"   -> "2"
  "three" -> "3"
  "four"  -> "4"
  "five"  -> "5"
  "six"   -> "6"
  "seven" -> "7"
  "eight" -> "8"
  "nine"  -> "9"
  _       -> ""

nums = [ "one" , "two" , "three" , "four" , "five" , "six" , "seven" , "eight" , "nine"]

calibration :: Parser String
calibration = concat <$> some go
  where go = (:[]) <$> digitChar 
                   <|> wordMap <$> choice (try . string <$> nums)
                   <|> "" <$ satisfy isAlpha

calibrationRev :: Parser String
calibrationRev = concat <$> some go
  where go = (:[]) <$> digitChar 
                   <|> wordMap .reverse <$> choice (try . string <$> (map reverse nums))
                   <|> "" <$ satisfy isAlpha

main :: IO ()
main = do
  input <- lines <$> readFile "inputs/1"

  -- part one
  print $ map (parse calibration "") input & rights 
        & map (\x -> [head x, last x])
        & map readInt & catMaybes
        & sum

  -- part two 
  print $ map (parse calibration "") input & rights 
        & map head
        & (\xs -> map (parse calibrationRev "") (map reverse input) & rights 
                  & map head
                  & zip xs
                  & map (\(x, y) -> [x, y]))
        & map readInt & catMaybes
        & sum