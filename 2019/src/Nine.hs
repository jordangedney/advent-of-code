module Nine (runIntcodeProgram, progMain) where

import Intcode (runIntcodeProgram)
import qualified Data.Map.Strict as Map
import Util (splitOn)

progMain :: IO ()
progMain = do
  input <- Map.fromList . zip [0..] . map read . splitOn "," <$> readFile "inputs/nine"
  print $ head $ runIntcodeProgram [1] input
  print $ head $ runIntcodeProgram [2] input
