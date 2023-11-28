import Lude

import qualified Data.Map as M

data Action = On | Off | Toggle deriving (Show, Eq)

main :: IO ()
main = do 
  input <- lines <$> readFile "inputs/6"

  -- part one
  let getAction = \case "toggle" -> Toggle; "off" -> Off; "on" -> On; e -> error (show e)
      getCoord :: String -> [Int]
      getCoord str = splitOn "," str & map read
      lightList [startX, startY] [endX, endY] = 
        [(x, y) | x <- [startX..endX], y <- [startY..endY]]
      lightList _ _ = error "fuck"
      parse xs = case words xs & reverse & take 4 of 
        [end, _, start,  action] -> (getAction action, lightList (getCoord start) (getCoord end))
        _ -> error "parsing is fucked"
      myLights :: Map (Int, Int) Action
      myLights = M.fromList $ map (, Off) (lightList [0, 0] [999, 999])
      toggleLight lights coord = case M.findWithDefault Off coord lights of
        Off -> M.insert coord On lights
        On  -> M.insert coord Off lights
        _   -> lights
      updateLights lights (todo, affected) = case todo of
        Off    -> foldl (\ls r -> M.insert r Off ls) lights affected
        On     -> foldl (\ls r -> M.insert r On  ls) lights affected
        Toggle -> foldl toggleLight lights affected

  print $ map parse input & foldl updateLights myLights & M.elems & filter (== On) & length

  -- part two
  let myLights' :: Map (Int, Int) Int
      myLights' = M.fromList $ map (, 0 :: Int) (lightList [0, 0] [999, 999])
      clampedDec a b = let res = a + b in max res 0
      updateBrightness :: Map (Int, Int) Int -> (Action, [(Int, Int)]) -> Map (Int, Int) Int
      updateBrightness lights (todo, affected) = case todo of
        Off    -> foldl (\ls r -> M.insertWith clampedDec r (-1) ls) lights affected
        On     -> foldl (\ls r -> M.insertWith (+) r 1 ls) lights affected
        Toggle -> foldl (\ls r -> M.insertWith (+) r 2 ls) lights affected
  print $ map parse input & foldl updateBrightness myLights' & M.elems & sum