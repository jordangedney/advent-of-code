-- | Let's see if we can't beat ghavil.

module Four where

import Util
import Data.List (isInfixOf)
import Data.Maybe (isJust)

main :: IO ()
main = do
  input <- readFile "inputs/four"
  print (partOne input)
  print (partTwo input)

partOne input =
  let passportEntries = splitOn "\n\n" input
      passportFields = map (++ ":") ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]
      hasEveryField entry = all (`isInfixOf` entry) passportFields
  in count True (map hasEveryField passportEntries)

partTwo input =
  let passportEntries = splitOn "\n\n" input
  in count True (map isValidPassport passportEntries)

readMaybe :: (Read a) => String -> Maybe a
readMaybe s = case reads s of
              [(x, "")] -> Just x
              _ -> Nothing

readIntMaybe :: String -> Maybe Int
readIntMaybe = readMaybe

isValidPassport entry =
  let components = words entry
      get' field = field ++ ":" & (\x -> filter (x `isInfixOf`) components)
      get field = case get' field of
                    [] -> ""
                    ((_:_:_:_:x):xs) -> x
                    _ -> ""
      intRange toCheck min' max' = case readIntMaybe toCheck of
                               Nothing -> Nothing
                               Just x -> if x >= min' && x <= max' then Just x else Nothing
      byr = intRange (get "byr") 1920 2002
      iyr = intRange (get "iyr") 2010 2020
      eyr = intRange (get "eyr") 2020 2030
      hgt =
        let isCM = "cm" `isInfixOf` get "hgt"
            isIN = "in" `isInfixOf` get "hgt"
            numericValue = get "hgt" & init & init
        in if isCM then intRange numericValue 150 193
           else if isIN
                then intRange numericValue 59 76
                else Nothing
      hcl = let r = get "hcl"
                hexNumber x = x `elem` (['0'..'9'] ++ ['a'..'f'])
            in if length r == 7
               then if head r == '#' && all hexNumber (tail r)
                    then Just r
                    else Nothing
               else Nothing
      ecl = if get "ecl" `elem` ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]
            then Just (get "ecl")
            else Nothing
      pid = if length (get "pid") == 9 && isJust (readIntMaybe (get "pid"))
            then Just (get "pid")
            else Nothing
  in case (byr, iyr, eyr, hgt, hcl, ecl, pid) of
          (Just _, Just _, Just _, Just _, Just _, Just _, Just _) -> True
          _ -> False
