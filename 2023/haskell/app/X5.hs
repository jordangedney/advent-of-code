import Lude
import Control.Applicative (liftA3)
import Data.List.Split (chunksOf)

entry :: Parser Int
entry = read <$> some digitChar <* many (char ' ')

parseMap :: String -> Parser [(Int, Int, Int)]
parseMap mapName =
  let entries = liftA3 (,,) entry entry entry <* newline
  in string (mapName <> " map:\n") *> some entries <* many newline

almanac :: Parser ([Int], [[(Int, Int, Int)]])
almanac = do
  seeds <- string "seeds: " >> some entry <* newline <* newline
  tables  <- mapM parseMap
               [ "seed-to-soil"
               , "soil-to-fertilizer"
               , "fertilizer-to-water"
               , "water-to-light"
               , "light-to-temperature"
               , "temperature-to-humidity"
               , "humidity-to-location"]
  pure (seeds, tables)

main :: IO ()
main = do
  let input = testInput
  input <- readFile "inputs/5"

  -- part one
  let [(seeds, rules)] = [parse almanac "" input] & rights

      mkTranslationFn rules input =
        let translate (destination, source, len) input =
              let distance = destination - source
                  inTable = source <= input && input <= (source + len - 1)
              in if inTable then Just (input + distance) else Nothing
        in head $ mapMaybe (($ input) . translate) rules ++ [input]

      seedToLocation = foldr (.) id (reverse [mkTranslationFn rs | rs <- rules])
      locations = map seedToLocation seeds
  print $ minimum locations

  -- part two
  let [(seeds, rules)] = [parse almanac "" input] & rights
      seeds' = chunksOf 2 seeds
      isSeed i = or [l <= i && i <= (l + r) |  [l, r] <- seeds']

      mkTranslationFn rules input =
        let translate (destination, source, len) input =
              let distance = destination - source
                  estimate = input - distance
                  inTable = source <= estimate && estimate <= source + len - 1
              in if inTable then Just (input - distance) else Nothing
        in head $ mapMaybe (($ input) . translate) rules ++ [input]

      locationToSeed = foldr ((.) . mkTranslationFn) id rules

  print $ head $ [x | x <- [1..], isSeed (locationToSeed x)]

testInput =
  [ "seeds: 79 14 55 13"
  , ""
  , "seed-to-soil map:"
  , "50 98 2"
  , "52 50 48"
  , ""
  , "soil-to-fertilizer map:"
  , "0 15 37"
  , "37 52 2"
  , "39 0 15"
  , ""
  , "fertilizer-to-water map:"
  , "49 53 8"
  , "0 11 42"
  , "42 0 7"
  , "57 7 4"
  , ""
  , "water-to-light map:"
  , "88 18 7"
  , "18 25 70"
  , ""
  , "light-to-temperature map:"
  , "45 77 23"
  , "81 45 19"
  , "68 64 13"
  , ""
  , "temperature-to-humidity map:"
  , "0 69 1"
  , "1 0 69"
  , ""
  , "humidity-to-location map:"
  , "60 56 37"
  , "56 93 4" ] & unlines
