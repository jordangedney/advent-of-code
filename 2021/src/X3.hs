module X3 (main) where

binToInt :: String -> Int
binToInt = foldl (\acc x -> acc * 2 + digitToInt x) 0

part1 :: [String] -> Int
part1 xs = mostCommon (>) (rotate xs)
  |> (\x -> binToInt x * binToInt (map invert x))
  where invert '1' = '0'
        invert '0' = '1'

mostCommon :: (Int -> Int -> Bool) -> [String] -> String
mostCommon cmp xs = xs
  |> map (foldr (\l (o, z) -> if l == '1' then (o + 1, z) else (o, z + 1)) (0,0))
  |> map (\(oneCount, zeroCount) -> if cmp oneCount zeroCount then '1' else '0')

getRating :: (Int -> Int -> Bool) -> Int -> [(Int, String)] -> [(Int, String)]
getRating cmp pos xs = map snd xs
  |> map (drop pos)
  |> rotate
  |> mostCommon cmp
  |> head
  |> (\c -> [(ind, val) | (ind, val) <- xs, (val !! pos) == c])
  |> (\ys -> if length ys == 1 then ys else getRating cmp (pos + 1) ys)

part2 :: [String] -> Int
part2 xs = zip [0..] xs
  |> (\x -> (getRating (>=) 0 x, getRating (<) 0 x))
  |> (\([(_, a)], [(_, b)]) -> binToInt a * binToInt b)

main :: IO ()
main = readFile "inputs/3" <&> lines >>> part2 >>= print
