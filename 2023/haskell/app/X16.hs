{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Fuse mapM_/map" #-}
{-# HLINT ignore "Use bimap" #-}
{-# HLINT ignore "Use map once" #-}
{-# HLINT ignore "Use uncurry" #-}
import Lude
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.List (sortBy)
import Data.Ord (comparing)
import Data.Foldable (maximumBy)
import Data.Maybe (fromJust)
import Data.List.Split (chunksOf)

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
  let parsed = map (parse (some part) "") input & rights & mkGrid
      start = ((0, 0), D) :: Beam

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

      foo  = run (Set.singleton start) (Set.singleton (0,0)) & drop 1500 & head

  print $ foo & length

  -- 8620 too high
  -- 8619 too high
  -- mapM_ print $ foo 

      -- run []    done = [([], done)]
      -- run beams done = 
      --   let toDo :: [Beam]
      --       toDo = concat [bs | (d, bs) <- map (step parsed) beams, d == KeepGoing]

      --       alsoDone :: Set.Set Coord
      --       alsoDone =
      --         let r = concat [bs | (d, bs) <- map (step parsed) beams, d == Done]
      --             combined = concatMap (\(c, _, been) -> c:been) r 
      --         in Set.fromList combined

      --   in  (toDo, done) : run toDo (Set.union alsoDone done)

      -- joined = 
      --   let [(bs, ran)] = run [start] Set.empty & drop 560 & take 1
      --       combined = concatMap (\(c, _, been) -> c:been) bs
      --   in Set.union ran (Set.fromList combined)

      -- foo = joined & length



  -- mapM_ print $ ran & map (\(xs, ys) -> (map fst' xs,  map thd ys))
  --                   & map (\(xs, ys) -> xs ++ concat ys)
  --                   & nub
    

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