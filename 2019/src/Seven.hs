{-# LANGUAGE RecordWildCards #-}

module Seven where

import Util (splitOn, replace, digits)
import Data.Bool (bool)
import Data.List (permutations)

data Mode = Immediate | Position deriving (Show, Eq)
data Opcode =
  Add | Multiply | Input | Output | JumpIfTrue | JumpIfFalse | LessThan | Equals | Halt
  deriving (Show, Eq)

getParameterMode :: Int -> (Mode, Mode)
getParameterMode op =
  let [_, b, c, _, _] = pad $ digits op
      pad xs = replicate (5 - length xs) 0 ++ xs
      mode x = bool Immediate Position (x == 0)
  in (mode c, mode b)

getOpcode :: Int -> Opcode
getOpcode op =
  case last (digits op) of
    1 -> Add
    2 -> Multiply
    3 -> Input
    4 -> Output
    5 -> JumpIfTrue
    6 -> JumpIfFalse
    7 -> LessThan
    8 -> Equals
    9 -> Halt
    _ -> error "Unexpected opcode!"

data IntcodeProgram =
  IntcodeProgram {pos :: Int, input :: [Int], code :: [Int]}

runIntcodeProgram :: [Int] -> [Int] -> [Int]
runIntcodeProgram input' program = go $ IntcodeProgram 0 input' program where
  go prg@IntcodeProgram{..} =
    let instruction = code !! pos
        [fstImmediate, sndImmediate, thdImmediate] = map ((code !!) . (+ pos)) [1..3]
        loadVar loadFrom immd = if loadFrom == Position then code !! immd else immd
        (fstMode, sndMode) = getParameterMode $ code !! pos
        (fstP, sndP) = (loadVar fstMode fstImmediate, loadVar sndMode sndImmediate)

        runOp op = go prg {pos = pos + 4, code = replace thdImmediate (op fstP sndP) code }

        jumpIf p = if p fstP then go prg {pos = pos + 3} else go prg {pos = sndP}

    in case getOpcode instruction of
      Add         -> runOp (+)
      Multiply    -> runOp (*)
      LessThan    -> runOp (\x y -> bool 0 1 (x < y))
      Equals      -> runOp (\x y -> bool 0 1 (x == y))
      JumpIfTrue  -> jumpIf (== 0)
      JumpIfFalse -> jumpIf (/= 0)
      Input       -> go prg {pos = pos + 2,
                            input = tail input,
                            code = replace fstImmediate (head input) code}
      Output      -> fstP : go prg {pos = pos + 2}
      Halt        -> []

runSequence :: [Int] -> (Int, Int, Int, Int, Int) -> [Int]
runSequence ampCode (a, b, c, d, e) = o5
  where o1 = runIntcodeProgram (a : 0 : o5) ampCode
        o2 = runIntcodeProgram (b : o1) ampCode
        o3 = runIntcodeProgram (c : o2) ampCode
        o4 = runIntcodeProgram (d : o3) ampCode
        o5 = runIntcodeProgram (e : o4) ampCode

progMain :: IO ()
progMain = do
  input <- map read . splitOn "," <$> readFile "inputs/seven"
  let toTup [a, b, c, d, e] = (a, b, c, d, e)
      toTup _ = error "Well fuck"
  print $ maximum . map (head . runSequence input . toTup) $ permutations [0..4]
  print $ maximum . map (last . runSequence input . toTup) $ permutations [5..9]
