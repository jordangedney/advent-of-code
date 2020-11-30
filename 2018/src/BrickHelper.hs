module BrickHelper where

import Util ((|>))
import Control.Monad (forever, void)
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent (threadDelay, forkIO)
import Data.Maybe (fromMaybe)

import Brick
  ( App(..), AttrMap, BrickEvent(..), EventM, Next, Widget
  , customMain, neverShowCursor , continue, halt, attrMap, AttrName,
  )

import Brick.BChan (newBChan, writeBChan)
import qualified Graphics.Vty as V

data Tick = Tick

mkApp :: (a -> [Widget ()]) -> (a -> a) -> App a Tick ()
mkApp drawUI updateState = App { appDraw = drawUI
            , appChooseCursor = neverShowCursor
            , appHandleEvent = handleEvent updateState
            , appStartEvent = return
            , appAttrMap = const theMap
            }

handleEvent :: (s -> s) -> s -> BrickEvent () Tick -> EventM () (Next s)
handleEvent step g (AppEvent Tick) = continue $ step g
handleEvent step g (VtyEvent (V.EvKey (V.KChar 'q') [])) = halt g
handleEvent step g (VtyEvent (V.EvKey V.KEsc [])) = halt g
handleEvent step g _ = continue g

theMap :: AttrMap
theMap = attrMap V.defAttr []

-- For simple apps which are only functions of time
stepBasedUI :: Int -> s -> (s -> s) -> (s -> [Widget ()]) -> IO ()
stepBasedUI timePerTickInMicroSeconds initialState updateState drawState = do
  chan <- newBChan 10
  forkIO $ forever $ do
    writeBChan chan Tick
    threadDelay timePerTickInMicroSeconds
  void $ customMain
         (V.mkVty V.defaultConfig) (Just chan) (mkApp drawState updateState) initialState

secondsToMicroSeconds = (1000000 *)
