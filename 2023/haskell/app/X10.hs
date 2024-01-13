import Lude
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.List (intersperse)

data Pipe = EN | NS | EW | NE | NW | SW | SE | GG | SS
  deriving (Show, Eq, Ord)

pipe :: Parser Pipe
pipe = parseWithMap tokensM
tokensM =
  [ (NS, '|'), (EW, '-'), (NE, 'L'), (NW, 'J'),
    (SW, '7'), (SE, 'F'), (GG, '.'), (SS, 'S'),
    (EN, '*') ]

findStart grid = M.filter (== SS) grid & M.keys & head

ddGet g c = M.findWithDefault GG c g

expandPipeMap input connectedToStart start =
  let emptyLine i = replicate (head i & length) '.'

      withBlankTopBottom = emptyLine input : input ++ [emptyLine input]
      withBlankLeftRight = map (\s -> '.' : s ++ ['.']) withBlankTopBottom
      addBlankLines = intersperse (emptyLine withBlankLeftRight) withBlankLeftRight
      addGapsBetween = map (intersperse '.') addBlankLines
      parsed = map (parse (some pipe) "") addGapsBetween & rights & mkGrid

      findStraight dirs atLoc =
        concat [c : map ($ c) dirs | (c, v) <- itoList parsed, v == atLoc]

      findCorner d1 x1 d2 x2 =
        [ k | (k, v) <- itoList parsed,
          v == GG, ddGet parsed (d1 k) == x1, ddGet parsed (d2 k) == x2 ]

      startExpanded = findStart parsed
      findStartConnection ds = [d startExpanded | d <- ds, d start `elem` connectedToStart]

      toUpdate =
        [ (EW, findStraight [left, right] EW)
        , (NS, findStraight [up,    down] NS)
        , (EW, findStraight [left, right] EW)
        , (NS, findStraight [up,    down] NS)
        , (EW, findCorner left NE right SW)
        , (EW, findCorner left SE right SW)
        , (NS, findCorner up SE down NE)
        , (NS, findCorner up SW down NW)
        , (NS, findCorner up SW down NE)
        , (NS, findCorner up SE down NW)
        , (EW, findCorner left SE right NW)
        , (EW, findCorner left NE right NW)
        , (EW, findStartConnection [left, right])
        , (NS, findStartConnection [up,    down]) ]

      updateCoords v xs m = foldr (`M.insert` v) m xs
      expandedMapWithNewlyAddedPipes = foldr (\(t, cs) x -> updateCoords t cs x) parsed toUpdate

  in expandedMapWithNewlyAddedPipes

pipeFlood m c = [dir c | (dir, isValidPipe) <- getters m, isValidPipe (dir c)]
  where isOneOf v c = ddGet m c `elem` v
        getters m = [ (up,    isOneOf [NS, SW, SE])
                    , (down,  isOneOf [NS, NE, NW])
                    , (right, isOneOf [EW, NW, SW])
                    , (left,  isOneOf [EW, NE, SE]) ]

flood next fstNode = go [fstNode] [fstNode]
  where go haveBeen toGo =
          let nextNodes = [x | x <- concatMap next toGo, x`notElem` haveBeen]
          in if null nextNodes then nub haveBeen
            else go (nextNodes ++ haveBeen) nextNodes

getMaps input =
  let parsed = map (parse (some pipe) "") input & rights & mkGrid
      parsedStart = findStart parsed
      connectedToStart = reverse (flood (pipeFlood parsed) parsedStart)
                       & take 3 & drop 1
      expanded = expandPipeMap input connectedToStart parsedStart
  in (parsed, parsedStart, expanded)

main :: IO ()
main = do
  let input = testInput6
  input <- lines <$> readFile "inputs/10"

  -- part one
  let (parsed, start, expanded) = getMaps input
      farthestPipe = flood (pipeFlood parsed) start & head

  print $ shortestPath parsed (pipeFlood parsed) start farthestPipe
        & length & (+ (-1))

  -- part two
  let pipeLoop grid =
        let start = findStart grid
            connected = flood (pipeFlood grid) start & take 2
        in concatMap (shortestPath grid (pipeFlood grid) start) connected & S.fromList

      groundFlood grid c = [x | x <- cross c, ddGet grid x == GG]
      notLoopFlood grid c =
        let loop = pipeLoop grid
        in cross c & filter (`S.notMember` loop)

      toExpanded (x, y) = (x * 2 + 2, y * 2 + 2)

      toTopLeft grid flood = shortestPath grid (flood grid) (0,0)
      noPathOutP grid fld coord = toTopLeft grid fld coord & null

      notInPipeLoop (c, v) grid = c `notElem` pipeLoop grid && v /= SS

      enclosedAnywhere = 
        let toFind k a = (k, a) `notInPipeLoop` parsed 
                       && noPathOutP expanded groundFlood (toExpanded k)
        in M.filterWithKey toFind parsed & M.map (const EN)

      insidePipeLoopBounds c =
        let scanDir d c = iterate d c & takeWhile (`M.member` parsed) & S.fromList
            searchDir d = not (null (scanDir d c `S.intersection` pipeLoop parsed))
        in all searchDir [left, right, up, down]

      enclosedInLoop = M.filterWithKey (\k a -> insidePipeLoopBounds k) enclosedAnywhere
                     & M.filterWithKey (\k a -> null (toTopLeft expanded notLoopFlood (toExpanded k)))
                     & (`M.union` parsed)

  print $ length (itoList (M.filter (== EN) enclosedInLoop))

testInput =
  [ "..F7."
  , ".FJ|."
  , "SJ.L7"
  , "|F--J"
  , "LJ..." ]

testInput2 =
  [ ".|..."
  , ".S-7."
  , ".|.|."
  , ".L-J."
  , "....." ]

testInput3 =
  [ "..........."
  , ".F-------7."
  , ".|F-----7|."
  , ".||.....||."
  , ".||.....||."
  , ".|L-7.F-J|."
  , ".|..|.|..|."
  , ".L--J.L--J."
  , "..........."
  , ".S-------7."
  , ".|F-----7|."
  , ".||.....||."
  , ".||.....||."
  , ".|L-7F--J|."
  , ".|..||...|."
  , ".L--JL---J."
  , "..........."
  , "...F-7....."
  , "...|.|....."
  , "...L-J....."
  , "..........." ]

testInput4 =
  [ ".F----7F7F7F7F-7...."
  , ".|F--7||||||||FJ...."
  , ".||.FJ||||||||L7...."
  , "FJL7L7LJLJ||LJ.L-7.."
  , "L--J.L7...LJS7F-7L7."
  , "....F-J..F7FJ|L7L7L7"
  , "....L7.F7||L7|.L7L7|"
  , ".....|FJLJ|FJ|F7|.LJ"
  , "....FJL-7.||.||||..."
  , "....L---J.LJ.LJLJ..." ]

testInput5 =
  [ "FF7FSF7F7F7F7F7F---7"
  , "L|LJ||||||||||||F--J"
  , "FL-7LJLJ||||||LJL-77"
  , "F--JF--7||LJLJ7F7FJ-"
  , "L---JF-JLJ.||-FJLJJ7"
  , "|F|F-JF---7F7-L7L|7|"
  , "|FFJF7L7F-JF7|JL---7"
  , "7-L-JL7||F7|L7F-7F7|"
  , "L.L7LFJ|||||FJL7||LJ"
  , "L7JLJL-JLJLJL--JLJ.L" ]

testInput6 =
  [ ".........."
  , ".S------7."
  , ".|.|....|."
  , ".|......|."
  , ".|F----7|."
  , ".||.|..||."
  , ".||....||."
  , ".||.F--J|."
  , ".LJ.L---J."
  , ".|---|.||." ]