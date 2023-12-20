import Lude
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

data Part = E | LM | RM | VS| HS deriving (Show, Eq, Ord)
data Dir = L | D | R | U deriving (Show, Eq, Ord)
type Beam = (Coord, Dir)
data Done = Done | KeepGoing deriving (Show, Eq, Ord)

part :: Parser Part
part = choice [ p <$ char c | (p, c) <-
  [ (E, '.'), (LM, '/'), (RM, '\\'), (VS, '|'), (HS, '-')] ]

main :: IO ()
main = do
  let input = testInput
  input <- lines <$> readFile "inputs/16"

  -- part one
  let parsed' = map (parse (some part) "") input & rights
      parsed = mkGrid parsed'

      step :: Grid Part -> Beam -> (Done, [Beam])
      step tiles b@(pos, dir) =
        let nextCoord = ($ pos) $ case dir of
              L -> left
              D -> down
              R -> right
              U -> up
        in case Map.lookup nextCoord tiles of
          Nothing -> (Done, [b])
          Just x -> case x of
            E -> (KeepGoing, [(nextCoord, dir)])
            LM ->
              let newDir = case dir of
                    L -> D
                    D -> L
                    R -> U
                    U -> R
              in (KeepGoing, [(nextCoord, newDir)])
            RM ->
              let newDir = case dir of
                    L -> U
                    D -> R
                    R -> D
                    U -> L
              in (KeepGoing, [(nextCoord, newDir)])
            VS ->
              let newDir = case dir of
                    L -> [U, D]
                    D -> [D]
                    R -> [U, D]
                    U -> [U]
              in (KeepGoing, [(nextCoord, nD) | nD <- newDir])
            HS ->
              let newDir = case dir of
                    L -> [L]
                    D -> [L, R]
                    R -> [R]
                    U -> [L, R]
              in (KeepGoing, [(nextCoord, nD) | nD <- newDir])

      run :: Set.Set Beam -> Set.Set Coord -> [Set.Set Coord]
      run beams done = if Set.null beams then [done] else
        let stepped = map (step parsed) (Set.toList beams)

            getCoords bs = Set.fromList (map fst bs)

            toDo :: [Beam]
            toDo = concat [bs | (d, bs) <- stepped, d == KeepGoing]

            newSeen :: Set.Set Coord
            newSeen = concat [bs | (d, bs) <- stepped] & getCoords
              & Set.union done
              & Set.union (Set.map fst beams)

        in  newSeen : run (Set.fromList toDo) newSeen

      safeInd _ [x] = x
      safeInd 0 (x:xs) = x
      safeInd n (x:xs) = safeInd (n - 1) xs

      runWith n = safeInd 800 (run (Set.singleton n) Set.empty)
                  & length
                  & (+ (-1))

  print $ runWith ((0, -1), R)

  -- part two
  let width = length (head parsed')
      height = length parsed'
      lefts = [((x, -1) , R) | x <- [0..height]]
      rights = [((x, width) , L) | x <- [0..height]]
      top = [((-1, x) , D) | x <- [0..width]]
      bottom = [((height, x) , U) | x <- [0..width]]
      all = concat [lefts, rights, top, bottom]
  print $ map runWith all & maximum

testInput =
  [ ".|...\\...."
  , "|.-.\\....."
  , ".....|-..."
  , "........|."
  , ".........."
  , ".........\\"
  , "..../.\\\\.."
  , ".-.-/..|.."
  , ".|....-|.\\"
  , "..//.|...." ]