import Lude

main :: IO ()
main = do
  input <- lines <$> readFile "inputs/2"

  -- part one
  let parse = splitOn "x" |> mapM readInt
      calcPaperAmt xs@[l, w, h] = 
        let minSideArea = sort xs & take 2 & product
        in Just $ 2*l*w + 2*w*h + 2*h*l + minSideArea
      calcPaperAmt _ =  Nothing
  print $ input & mapMaybe (parse >=> calcPaperAmt) & sum

  -- part two
  let calRibbonAmt xs = 
        let smallestPerim = sort xs & take 2 & map (* 2) & sum
        in Just $ product xs + smallestPerim
  print $ input & mapMaybe (parse >=> calRibbonAmt) & sum
