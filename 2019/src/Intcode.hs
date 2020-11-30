{-# LANGUAGE RecordWildCards #-}

module Intcode (runIntcodeProgram) where

import Util (splitOn, digits)
import Data.Bool (bool)
import qualified Data.Map.Strict as Map

data Mode = Immediate | Position | Relative deriving (Show, Eq)
data Opcode
  = Add | Multiply | Input | Output | JumpIfTrue | JumpIfFalse | LessThan | Equals
  | OffsetRelativeBase | Halt
  deriving (Show, Eq)

pad :: [Int] -> [Int]
pad xs = replicate (5 - length xs) 0 ++ xs

getParameterMode :: Int -> (Mode, Mode, Mode)
getParameterMode op =
  let [a, b, c, _, _] = pad $ digits op
      mode x = case x of
                 0 -> Position
                 1 -> Immediate
                 2 -> Relative
                 _ -> error "Unexpected parameter mode!"
  in (mode c, mode b, mode a)

getOpcode :: Int -> Opcode
getOpcode op = let [_, _, _, a, b] = pad $ digits op in
  case b of
    1 -> Add
    2 -> Multiply
    3 -> Input
    4 -> Output
    5 -> JumpIfTrue
    6 -> JumpIfFalse
    7 -> LessThan
    8 -> Equals
    9 -> case a of
        0 -> OffsetRelativeBase
        9 -> Halt
        _ -> error "Unexpected opcode!"
    _ -> error "Unexpected opcode!"

data IntcodeProgram =
  IntcodeProgram {pos :: Int, input :: [Int], code :: Map.Map Int Int, relativeBase :: Int}

runIntcodeProgram :: [Int] -> Map.Map Int Int -> [Int]
runIntcodeProgram input' program = go $ IntcodeProgram 0 input' program 0 where
  go prg@IntcodeProgram{..} =
    let instruction = code Map.! pos
        [fstImmediate, sndImmediate, thdImmediate] =
          map ((\x -> Map.findWithDefault 0 x code) . (+ pos)) [1..3]
        loadVar loadFrom immd =
          case loadFrom of
            Immediate -> immd
            Position  -> Map.findWithDefault 0 immd code
            Relative  -> Map.findWithDefault 0 (relativeBase + immd) code
        (fstMode, sndMode, thdMode) = getParameterMode instruction
        (fstP, sndP) = (loadVar fstMode fstImmediate, loadVar sndMode sndImmediate)

        updateProg pos_ with = Map.insert pos_ with code

        runOp op =
         let positionToUpdate =
               if thdMode == Relative then relativeBase + thdImmediate else thdImmediate
         in go prg {pos = pos + 4, code = updateProg positionToUpdate (op fstP sndP)}

        jumpIf p = if p fstP then go prg {pos = pos + 3} else go prg {pos = sndP}

    in case getOpcode instruction of
      Add                -> runOp (+)
      Multiply           -> runOp (*)
      LessThan           -> runOp (\x y -> bool 0 1 (x < y))
      Equals             -> runOp (\x y -> bool 0 1 (x == y))
      JumpIfTrue         -> jumpIf (== 0)
      JumpIfFalse        -> jumpIf (/= 0)
      Input              ->
        let positionToUpdate = if fstMode == Relative then relativeBase + fstImmediate
                               else fstImmediate
        in go prg { pos   = pos + 2
                  , input = tail input
                  , code  = updateProg positionToUpdate (head input)}
      Output             -> fstP : go prg {pos = pos + 2}
      OffsetRelativeBase -> go prg {pos = pos + 2, relativeBase = relativeBase + fstP}
      Halt               -> []
