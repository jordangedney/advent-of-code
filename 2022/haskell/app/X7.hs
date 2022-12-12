import qualified Data.Map as M

data File
  --    Name   Parents         Name    Size
  = Dir String [String] | Node String  Int
  deriving (Eq, Ord, Show)

parse :: (M.Map File [File]) -> File -> [String] -> (M.Map File [File])
parse filesystem _ [] = filesystem
parse filesystem curDir@(Dir position ps@(p:parents)) (r:remaining) =
  case (words r) of
    ["$", "cd", ".."] ->
      let [parentDir] = [x | x@(Dir pos rents) <- M.keys filesystem, p == pos && rents == parents]
      in parse filesystem parentDir remaining
    ["$", "cd", dirName] -> parse filesystem (Dir dirName newRents) remaining
    ["$", "ls"] -> parse filesystem curDir remaining
    ["dir", dirName] ->
      let newDir = [Dir dirName newRents]
          updatedFS = M.insertWith (++) curDir newDir filesystem
      in parse updatedFS curDir remaining
    [size, filename] ->
      let newFile = [Node filename (read size)]
          updatedFS = M.insertWith (++) curDir newFile filesystem
      in parse updatedFS curDir remaining
  where newRents = position:ps

totalSize :: (M.Map File [File]) -> File -> Int
totalSize filesystem dir =
  let getsize (Node _ s) = s
      getsize           d =  totalSize filesystem d
  in sum (map getsize (filesystem M.! dir))

main :: IO ()
main = do
  input <- tail <$> lines <$> readFile "../inputs/7.test"

  let fs = parse (M.fromList [(Dir "/" ["/"], [])]) (Dir "/" ["/"]) input
      dirsWithSize = (map ((\e -> (e, totalSize fs e))) (M.keys fs))

  print $ sum [s | (Dir _ _,  s) <- dirsWithSize, s <= 100000]
