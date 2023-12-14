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
import Data.Map (fromList)
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
      eventsMap = fromList [((0, 0), initialEvents)],
      iChoice = -1,
      inEvent = Nothing,
      monstersMap =
        fromList
          [ ( (0, 0),
              [Monster 5 5 "Goblin Raider" 30 8, Monster 5 10 "Forest Nymph" 20 6, Monster 5 15 "Mountain Troll" 50 10, Monster 10 5 "Shadow Assassin" 40 12]
            )
          ],
      inMonster = Nothing,
      gameOver = False,
      mountainsMap = fromList [((0, 0), [Mountain 1 2, Mountain 1 3, Mountain 1 4, Mountain 1 5, Mountain 2 5])],
      -- set it to True to display debug messages
      displayLogs = False,
      logs = []
    }

initialEvents :: [GameEvent]
initialEvents =
  [ sleepEvent,
    goblinRaiderEvent,
    forestNymphEvent,
    mountainTrollEvent,
    shadowAssassinEvent,
    treasureChest,
    ancientShrineEncounter,
    mysteriousTraveler,
    lostTreasureChest,
    enchantedLake,
    ancientLibrary
  ]