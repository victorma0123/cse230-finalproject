module Init where

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
import System.Random (randomRIO)
import Control.Concurrent (forkIO, threadDelay)
import Control.Monad (forever)
import GameLogic (moveMonster, monsterEncounterEvent, treasureChest)

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
      monsters = [Monster 10 10, Monster 10 20],
      gameOver = False
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