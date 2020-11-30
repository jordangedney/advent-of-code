{-# LANGUAGE RecordWildCards #-}

module Five where

import Util (splitOn, replace, digits)
import Data.Bool (bool)

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

data IntcodeProgram = IntcodeProgram {pos :: Int, output :: [Int], code :: [Int]}

runIntcodeProgram :: Int -> [Int] -> ([Int], [Int])
runIntcodeProgram input program = go $ IntcodeProgram 0 [] program where
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
      Input       -> go prg {pos = pos + 2, code = replace fstImmediate input code}
      Output      -> go prg {pos = pos + 2, output = fstP : output}
      Halt        -> (code, output)

progMain :: IO ()
progMain = do
  input <- map read . splitOn "," <$> readFile "inputs/five"
  print $ head . snd $ runIntcodeProgram 1 input
  print $ head . snd $ runIntcodeProgram 5 input
