module Main where

import Brick
  ( App (..),
    attrMap,
    customMain,
    neverShowCursor,
  )
import Brick.BChan (newBChan, writeBChan)
import Brick.Widgets.Core
import qualified Graphics.Vty as V
import Types
import UI
import System.Random (randomRIO)
import Control.Concurrent (forkIO, threadDelay)
import Control.Monad (forever)
import GameLogic 
import Init
import Control.Monad

app :: App Game Tick Name
app =
  App
    { appDraw = drawUI,
      appChooseCursor = neverShowCursor,
      appHandleEvent = handleEvent,
      appStartEvent = return,
      appAttrMap = const theMap 
    }

main :: IO ()
main = do
  eventChan <- newBChan 10
  _ <- forkIO $ forever $ do
    writeBChan eventChan Tick
    threadDelay 900000  -- 0.5秒间隔

  let buildVty = V.mkVty V.defaultConfig
  initialVty <- buildVty
  finalState <- customMain initialVty buildVty (Just eventChan) app initialState
  when (gameOver finalState) $ do
    putStrLn "Game Over! Thank you for playing."
  return ()