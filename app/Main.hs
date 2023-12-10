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
    { posX = 0,
      posY = 0,
      sheild = 100,
      sword = 100,
      hp = 100,
      attack = 100,
      events = initialEvents,
      iChoice = -1,
      inEvent = Nothing,
      monsters = [Monster 10 10, Monster 10 20]
    }

initialEvents :: [GameEvent]
initialEvents =
  [ GEvent
      { eventX = 5,
        eventY = 5,
        name = "sleep!",
        description = "Sleeping will help recover HP",
        choices =
          [ GChoice
              { title = "sleep for 10 hours",
                effect = \g -> g {hp = hp g + 2}
              },
            GChoice
              { title = "sleep for 5 hours",
                effect = \g -> g {hp = hp g + 1}
              }
          ],
        icon = str "s"
      }, monsterEncounterEvent, treasureChest
  ]

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
  return ()

