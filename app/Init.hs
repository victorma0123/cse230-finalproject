module Init where

import Brick
  ( App (..),
    attrMap,
    customMain,
    neverShowCursor,
  )
import Brick.BChan (newBChan, writeBChan)
import Brick.Widgets.Core
import Control.Concurrent (forkIO, threadDelay)
import Control.Monad (forever)
import GameLogic
import qualified Graphics.Vty as V
import System.Random (randomRIO)
import Types

-- The coordinates in the game. For now, the x axis is from left to right,
-- and the y axis is from top to buttom.
--   -----------------------x axis--------------------
--   | ......
--   | ......
-- y axis
--   | ......
--   | ......

initialState :: Game
initialState =
  Game
    { posX = 0,
      posY = 0,
      shield = 10,
      sword = 10,
      hp = 100,
      attack = 15,
      events = initialEvents,
      iChoice = -1,
      inEvent = Nothing,
      monsters = [Monster 10 10 "Goblin Raider", Monster 5 5 "Forest Nymph"],
      inMonster = Nothing,
      gameOver = False,
      mountains = [Mountain 1 2, Mountain 1 3, Mountain 1 4, Mountain 1 5, Mountain 2 5]
    }

initialEvents :: [GameEvent]
initialEvents =
  [ sleepEvent,
    goblinRaiderEvent,
    forestNymphEvent,
    mountainTrollEvent,
    shadowAssassinEvent,
    treasureChest
  ]