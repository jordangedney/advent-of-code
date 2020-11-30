module One where

fuelForModule :: Double -> Integer
fuelForModule = (+ (- 2)) . floor . (/ 3.0)

totalFuel :: Double -> Double
totalFuel = sum . takeWhile (> 0) . drop 1 . iterate (fromIntegral . fuelForModule)

progMain :: IO ()
progMain = do
  input <- map read . lines <$> readFile "inputs/one"
  print $ sum . map fuelForModule $ input
  print $ sum . map totalFuel $ input
