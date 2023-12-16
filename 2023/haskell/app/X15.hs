import Lude
import Data.Char (ord, isAlpha)
import Data.Maybe (fromJust)
import qualified Data.Map.Strict as Map

data Op = D | E Int deriving (Show, Eq)

hashString :: String -> Int
hashString = foldl hash 0
  where hash v c = ((v + ord c) * 17) `rem` 256

getLabel :: Parser (String, Op)
getLabel = do
  boxLabel <- some (satisfy isAlpha)
  op <- char '-' <|> char '='
  v <- optional entry
  pure (boxLabel, case op of '-' ->  D; _ -> E (fromJust v))

main :: IO ()
main = do
  let input = testInput 
  input <- head . lines <$> readFile "inputs/15"

  -- part one
  let parsed = input & splitOn ","
  print $ map hashString parsed & sum

  -- part two
  let evenParsier = map (parse getLabel "") parsed & rights

      removeLabel _ [] = []
      removeLabel label ((l, i):xs) 
        | label == l = xs  
        | otherwise = (l,i) : removeLabel label xs

      addLabel :: (String, Int) -> [(String, Int)] -> [(String, Int)]
      addLabel l [] = [l]
      addLabel (label, val) ((l, i):xs)
        | label == l = (l, val) : xs  
        | otherwise = (l,i) : addLabel (label, val) xs

      addBox bxs l@(label, val) = 
        case Map.lookup (hashString label) bxs of 
          Just xs -> Map.insert (hashString label) (addLabel l xs) bxs
          Nothing -> Map.insert (hashString label) [l] bxs

      removeBox :: Map.Map Int [(String, Int)] -> String -> Map.Map Int [(String, Int)]
      removeBox bxs label = 
        case Map.lookup (hashString label) bxs of 
          Just xs -> Map.insert (hashString label) (removeLabel label xs) bxs
          Nothing -> bxs

      doOp bxs (label, D)   = removeBox bxs label
      doOp bxs (label, E x) = addBox bxs (label, x) 
      
      doOps = foldl doOp Map.empty evenParsier & Map.toList

  print $ [(bNum + 1) * s * v | (bNum, lenses) <- doOps, (s, (lbl, v)) <- zip [1..] lenses]
        & sum

testInput = "rn=1,cm-,qp=3,cm=2,qp-,pc=4,ot=9,ab=5,pc-,pc=6,ot=7"