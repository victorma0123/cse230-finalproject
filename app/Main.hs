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
import GameLogic (moveMonster, monsterEncounterEvent, treasureChest)
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

  -- Set up a background process for periodically sending Tick events
  _ <- forkIO $ forever $ do
    writeBChan eventChan Tick
    threadDelay 3000000  -- 3 second intervals, adjust as needed

  let buildVty = V.mkVty V.defaultConfig
  initialVty <- buildVty
  finalState <- customMain initialVty buildVty (Just eventChan) app initialState
  when (gameOver finalState) $ do
    putStrLn "Game Over! Thank you for playing."
  return ()
