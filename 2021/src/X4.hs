module X4 (main) where

import Data.Vector (fromList, (//), (!), indexed, toList)

numCol = 5

parse xs =
  let numsToCall :: [Int] = head xs |> splitOn "," |> map read
      boards :: [[Int]] =
        tail (tail xs)
        |> splitOn [""] |> map (map (map read . words)) |> map concat
  in (numsToCall, boards)

type Game = (Vector Bool, [Int])

boardsWithHits = map (\b -> (fromList (replicate (length b) False), b))

doHits :: Int -> Game -> Vector Bool
doHits called (hits, board) = zip [0..] board
  |> (\x -> [(i, True) | (i, v) <- x, v == called])
  |> (hits //)

runGame :: [Int] -> [Game] -> [(Int, [Game])]
runGame [] _ = []
runGame (n:ns) bds =
  let updated = map (\(h, nums) -> (doHits n (h, nums), nums)) bds
  in (n, updated) : runGame ns updated

winningInds = [ [0..4]
              , [numCol..numCol*2 - 1]
              , [numCol*2..numCol*3 - 1]
              , [numCol*3..numCol*4 - 1]
              , [numCol*4..numCol*5 - 1]
              , take 5 [0,5..]
              , take 5 [1,6..]
              , take 5 [2,7..]
              , take 5 [3,8..]
              ]

doMath winningBoard =
  let (winningNum, (called, board)) = winningBoard
      calledInd = [i | (i, v) <- toList (indexed called), v]
      uncalled = [x | (i, x) <- zip [0..] board, i `notElem` calledInd]
  in winningNum * sum uncalled

winner :: Game -> Bool
winner (game, _ ) =
  let getIndc xs = [game ! x | x <- xs]
      check :: [[Bool]]
      check = map getIndc winningInds
  in any and check

part1 (numsToCall, boards) =
  let findWinner ((lastCalled, games):xs) =
        let winners = [g | g <- games, winner g]
            won = not $ null winners
        in if won then (lastCalled, head winners) else findWinner xs
  in runGame numsToCall (boardsWithHits boards) |> findWinner |> doMath

part2 (numsToCall, boards) =
  let keepTruckn l@(_, b) ((lastCalled, games):xs) =
        let winners = [g | g <- games, winner g]
            won = length winners == length games
            finally = [g | g@(_, b') <- games, b == b']
        in if won then (lastCalled, head finally) else keepTruckn l xs

      findWinner ((lastCalled, games):xs) =
        let losers = [g | g <- games, not (winner g)]
            over = length losers == 1
        in if over then keepTruckn (head losers) xs else findWinner xs

  in runGame numsToCall (boardsWithHits boards) |> findWinner |> doMath

main :: IO ()
main = readFile "inputs/4" <&> lines >>> parse >>> part2 >>= print
