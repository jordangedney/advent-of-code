module X8 (main) where

import Lude

parse = map (map sort . words) . splitOn "|"

getRules codes =
  let one    = numHas [(2 `sides`)]
      seven  = numHas [(3 `sides`)]
      four   = numHas [(4 `sides`)]
      eight  = numHas [(7 `sides`)]
      nine   = numHas [(6 `sides`), (`contains` one), (`contains` four)]
      zero   = numHas [(6 `sides`), (`contains` one), (`missing` four)]
      six    = numHas [(6 `sides`), (`missing` one), (`missing` four)]
      three  = numHas [(5 `sides`), (`contains` one)]
      five   = numHas [(5 `sides`), (nine `contains`), (`missing` three)]
      two    = numHas [(5 `sides`), (nine `missing`)]
  in zip [0..] [zero, one, two, three, four, five, six, seven, eight, nine]
  where a `contains` b = all (`elem` a) b
        a `missing` b = not (a `contains` b)
        x `sides` y = length y == x
        numHas ps = head [x | x <- codes, foldl1 (&&) (map ($ x) ps)]

translate [in', out] = [v | o <- out, (v, r) <- getRules (in' <> out), o == r]

-- part1 = sum . map length . map (filter (`elem` [1, 7, 4, 8]))
part2 = sum . map fromDigits

main :: IO ()
main = readFile "inputs/8" <&> lines >>> map (translate . parse) >>> part2 >>= print
