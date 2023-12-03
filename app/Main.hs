module Main where

import Brick
  ( App (..),
    attrMap,
    customMain,
    neverShowCursor,
  )
import Brick.BChan (newBChan)
import qualified Graphics.Vty as V
import Types
import UI

app :: App Game Tick Name
app =
  App
    { appDraw = drawUI,
      appChooseCursor = neverShowCursor,
      appHandleEvent = handleEvent,
      appStartEvent = return,
      appAttrMap = const $ attrMap V.defAttr []
    }

initialState :: Game
initialState =
  Game
    { x = 0,
      y = 0,
      sheild = 100,
      sword = 100,
      hp = 100,
      attack = 100
    }

main :: IO ()
main =
  do
    eventChan <- Brick.BChan.newBChan 10
    let buildVty = V.mkVty V.defaultConfig
    initialVty <- buildVty
    finalState <-
      customMain
        initialVty
        buildVty
        (Just eventChan)
        app
        initialState
    return ()