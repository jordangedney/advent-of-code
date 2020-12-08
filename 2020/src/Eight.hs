-- | Spontaneously developed boot loops have killed many a device.

module Eight where

import Util

main :: IO ()
main = do
  input <- map parseInstruction . lines <$> readFile "inputs/eight"
  print (partOne input)
  print (partTwo input)
  return ()

data Instruction = ACC Int | JMP Int | NOP Int deriving Show

parseInstruction instruction =
  let [op, arg] = words instruction
      opcode "acc" = ACC
      opcode "jmp" = JMP
      opcode _     = NOP
  in (opcode op) (read (if head arg == '+' then tail arg else arg) :: Int)

partOne input = fst (runInstructionsP1 0 0 input [])
runInstructionsP1 accumulator currentPosition instructions alreadySeen =
  let (acc, pos) = case instructions !! currentPosition of
                     ACC x -> (accumulator + x, currentPosition + 1)
                     JMP x -> (accumulator, currentPosition + x)
                     NOP _ -> (accumulator, currentPosition + 1)
  in if not (pos `elem` alreadySeen)
     then runInstructionsP1 acc pos instructions (pos:alreadySeen)
     else (acc, pos)

runInstructions accumulator currentPosition termPosition hacked instructions alreadySeen =
  let currentInstruction = instructions !! currentPosition

      doOP (ACC x) = (accumulator + x, currentPosition + 1)
      doOP (JMP x) = (accumulator, currentPosition + x)
      doOP (NOP _) = (accumulator, currentPosition + 1)

      doOPFlipped (ACC x) = (accumulator + x, currentPosition + 1)
      doOPFlipped (JMP _) = (accumulator, currentPosition + 1)
      doOPFlipped (NOP x) = (accumulator, currentPosition + x)

      properlyTerminated = currentPosition == termPosition

      result = (acc, pos, properlyTerminated)
      (acc, pos) = doOP currentInstruction

      resultF = (accF, posF, properlyTerminated)
      (accF, posF) = doOPFlipped currentInstruction

  in if pos `elem` alreadySeen || properlyTerminated
     then result
     else if hacked
          then runInstructions acc pos termPosition hacked instructions (pos:alreadySeen)
          else
            let flipped = runInstructions accF posF termPosition True instructions (pos:alreadySeen)
                unflipped = runInstructions acc pos termPosition False instructions (pos:alreadySeen)
            in if ((\(a, b, c) -> c) flipped) == True
               then flipped
               else unflipped

partTwo input = (runInstructions 0 0 (length input - 1) False input [])
