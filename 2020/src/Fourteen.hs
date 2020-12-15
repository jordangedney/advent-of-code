-- | 

module Fourteen where

import Util
import qualified Data.Bits as DB
import Data.List.Split (chunksOf)
import qualified Data.Map as Map
import Data.List (isInfixOf)

main :: IO ()
main = do
  input <- parse <$> readFile "inputs/fourteen"
  -- input <- parse <$> readFile "inputs/fourteenfoo"
  -- print (partOne input)
  print (partTwo input)

parse :: String -> [(String, Int, Int)]
parse file
  = [l | l <- splitOn "mask =" file, l /= ""]
  & map (splitOn "mem[")
  & map unwords
  & map (splitOn "] =")
  & map (words . unwords)
  & map (\(mask:updates) -> (mask, map readInt updates & chunksOf 2))
  & map (\(mask, updates) -> (map (\[x, y] -> (mask, x, y)) updates))
  & concat

complement = map (\x -> if x == 0 then 1 else 0)

x `nand` y = (x DB..&. (DB.complement y))

toDecimal :: [Int] -> Int
toDecimal = foldl (\acc x -> acc * 2 + x) 0

maskWithX _ [] = []
maskWithX x ('X':xs) = x : maskWithX x xs
maskWithX x (c:cs) = (readInt [c]) : maskWithX x cs

applyBitmask (mask, mem, val) =
  let s1 = toDecimal (maskWithX 0 mask) DB..|. val
      forceZero =
        mask &  maskWithX 1 & complement & toDecimal
      in (mem, s1 `nand` forceZero)

partOne input
  = map applyBitmask input
  & Map.fromList
  & Map.elems
  & sum

toBinary :: Int -> [Int]
toBinary 0 = [0]
toBinary n = toBinary (n `quot` 2) ++ [n `rem` 2]

toBin36 n = toBinary n & reverse & (++ (repeat 0)) & take 36 & reverse

wildCardMask ('0':xs) (y:ys) =  y : wildCardMask xs ys
wildCardMask ('1':xs) (y:ys) = '1' : wildCardMask xs ys
wildCardMask ('X':xs) (y:ys) = 'X' : wildCardMask xs ys
wildCardMask _ x = x

maskToAddresses :: String -> [String]
maskToAddresses address =
  let go [] seen = [seen]
      go ('X':xs) seen = [seen ++ "0" ++ xs, seen ++ "1" ++ xs]
      go (x:xs) seen = go xs (seen ++ [x])

      soLazy :: [String] -> [String]
      soLazy xs =
        let res = map (\x -> go x []) xs
        in if any (isInfixOf "X") xs
           then soLazy (concat res)
           else xs

  in soLazy [address]

toIntArray [] = []
toIntArray ('0':xs) = 0 : toIntArray xs
toIntArray ('1':xs) = 1 : toIntArray xs

partTwo input =
  input
  & map (\(a, b, c) -> (a, toBin36 b & map show & concat, c))
  & map (\(a, b, c) -> (wildCardMask a b, c))
  & map (\(a, b) -> (maskToAddresses a, b))
  & map (\(a, b) -> (map (toDecimal . toIntArray) a, b))
  & map (\(a, b) -> [(x, b) | x <- a])
  & concat
  & Map.fromList
  & Map.elems
  & sum
