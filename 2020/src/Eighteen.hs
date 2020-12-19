-- | -----------------------------------------------------------------------------------------------
-- | Reading this file has been known to cause brain damage.
-- | -----------------------------------------------------------------------------------------------

module Eighteen where

import Util
import Data.List (elemIndex)

main :: IO ()
main = do
  input <- map parse . lines <$> readFile "inputs/eighteen"
  -- print (partOne input)
  print (partTwo input)

data Token = Plus | Mult | LeftParen | RightParen | Val Int deriving (Show, Eq)

parseToken "+" = Plus
parseToken "*" = Mult
parseToken "(" = LeftParen
parseToken ")" = RightParen
parseToken x = Val (readInt x)

parse = map parseToken . words

(+++) = (+)
infixl 8 +++

evalNoParens' [Val x] = x
evalNoParens' (Val x : Plus : Val y : Mult : xs) = (x +++ y) * evalNoParens' xs
evalNoParens' (Val x : Plus : Val y : Plus : xs) = (x +++ y) +++ evalNoParens' xs
evalNoParens' (Val x : Mult : Val y : Mult : xs) = x * y * evalNoParens' xs
evalNoParens' (Val x : Mult : Val y : Plus : xs) = x * (y +++ evalNoParens' xs)
evalNoParens' (Val x : Plus : Val y:[]) = x +++ y
evalNoParens' (Val x : Mult : Val y:[]) = x * y
-- evalNoParens' (Val x : Plus : xs) =  x +++ evalNoParens' xs
-- evalNoParens' (Val x : Mult : xs) =  x * evalNoParens' xs
evalNoParens' e =  error (show e)

evalNoParens = evalNoParens' . reverse

doParens :: [Token] -> [Token]
doParens xs =
  case elemIndex RightParen xs of
    Just x ->
      let beforeRev = take x xs & reverse
          after = drop (x + 1) xs
      in case elemIndex LeftParen beforeRev of
        Just y ->
          let insideParens = reverse (take y beforeRev)
              outsideParen = reverse (drop (y + 1) beforeRev)
              result = [Val (eval insideParens)]
          in outsideParen ++ result ++ after
        Nothing -> error "No matching paren found"
    Nothing -> xs

eval xs =
  if RightParen `elem` xs
  then eval (doParens xs)
  else if Plus `elem` xs
       then eval (doAddition xs)
       -- else xs
       else evalNoParens xs

doAddition :: [Token] -> [Token]
doAddition xs =
  case elemIndex Plus xs of
    Just x ->
      let (Val before) = xs !! (x - 1)
          (Val after) = xs !! (x + 1)
          result = [Val (before + after)]
      in (take (x - 1) xs) ++ result ++ (drop (x + 2) xs)
    Nothing -> xs

-- partOne input = map eval input & sum
partTwo input = map eval input & sum
