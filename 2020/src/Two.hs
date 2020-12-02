-- | I love how they _need_ you to know what a tobaggan is.
-- | Or maybe they just want to introduce the bum slider.

module Two (main, partOne, partTwo, mkPasswordEntry) where

import Util

main :: IO ()
main = do
  input <- map mkPasswordEntry . lines <$> readFile "inputs/two"
  print (partOne input)
  print (partTwo input)

partOne input = input & map validPasswordEntry & count True

partTwo input = input & map moreValidPasswordEntry & count True

data PasswordEntry = PasswordEntry Int Int Char String

mkPasswordEntry entry =
  let [range, enforcedLetter:_, password] = splitOn " " entry
      [min, max] = splitOn "-" range & map read
  in PasswordEntry min max enforcedLetter password

validPasswordEntry (PasswordEntry min max enforcedLetter password) =
  count enforcedLetter password >= min &&
  count enforcedLetter password <= max

moreValidPasswordEntry (PasswordEntry min max enforcedLetter password) =
  (password !! (min - 1) == enforcedLetter) /=
  (password !! (max - 1) == enforcedLetter)
