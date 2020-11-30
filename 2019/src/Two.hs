module Two where

import Util (replace, splitOn)

runIntcodeProgram :: [Int] -> [Int]
runIntcodeProgram = go 0
  where go pos program =
          let [opCode, posOne, posTwo, posResult] = map ((program !!) . (+ pos)) [0..3]
              (a, b) = (program !! posOne, program !! posTwo)
              iterateWith opFn = go (pos + 4) (replace posResult (opFn a b) program)
          in case opCode of
            1 -> iterateWith (+)
            2 -> iterateWith (*)
            99 -> program
            _ -> error "Unrecognized opcode!"

runWith :: Int -> Int -> [Int] -> Int
runWith noun verb = (!! 0) . runIntcodeProgram . replace 1 noun . replace 2 verb

partTwo :: Int -> [Int] -> Int
partTwo lookingFor input =
  let bruteForce = [(n, v) | n <- [1..100], v <- [1..100], runWith n v input == lookingFor]
      [(noun, verb)] = take 1 bruteForce
  in 100 * noun + verb

progMain :: IO ()
progMain = do
  input <- map read . splitOn "," <$> readFile "inputs/two"
  print $ runWith 12 2 input
  print $ partTwo 19690720 input
