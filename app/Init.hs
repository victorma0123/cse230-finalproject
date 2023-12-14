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
      monsters = [Monster 9 4 "Goblin Raider" 30 8, Monster 26 4 "Forest Nymph" 20 6, Monster 9 14 "Mountain Troll" 50 10, Monster 26 14 "Shadow Assassin" 40 12],
      inMonster = Nothing,
      gameOver = False,
      mountains = [Mountain 17 12, Mountain 18 12, Mountain 19 12, Mountain 17 13, Mountain 19 13, Mountain 17 14, Mountain 18 14, Mountain 19 14,
             -- New mountains for Goblin Raider
             Mountain 5 1, Mountain 6 1, Mountain 7 1, Mountain 8 1, Mountain 9 1, Mountain 10 1, Mountain 11 1, Mountain 12 1,
             Mountain 5 2, Mountain 12 2, Mountain 5 3, Mountain 12 3, Mountain 5 4, Mountain 12 4, Mountain 5 5, Mountain 12 5,
             Mountain 5 6, Mountain 6 6, Mountain 7 6, Mountain 8 6, Mountain 10 6, Mountain 11 6, Mountain 12 6,
             -- New mountains for Forest Nymph
             Mountain 22 1, Mountain 23 1, Mountain 24 1, Mountain 25 1, Mountain 26 1, Mountain 27 1, Mountain 28 1, Mountain 29 1,
             Mountain 22 2, Mountain 29 2, Mountain 22 3, Mountain 29 3, Mountain 22 4, Mountain 29 4, Mountain 22 5, Mountain 29 5,
             Mountain 22 6, Mountain 23 6, Mountain 24 6, Mountain 25 6, Mountain 27 6, Mountain 28 6, Mountain 29 6,
             -- New mountains for Mountain Troll
             Mountain 5 11, Mountain 6 11, Mountain 7 11, Mountain 8 11, Mountain 10 11, Mountain 11 11, Mountain 12 11,
             Mountain 5 12, Mountain 12 12, Mountain 5 13, Mountain 12 13, Mountain 5 14, Mountain 12 14, Mountain 5 15, Mountain 12 15,
             Mountain 5 16, Mountain 6 16, Mountain 7 16, Mountain 8 16, Mountain 9 16, Mountain 10 16, Mountain 11 16, Mountain 12 16,
             -- New mountains for Shadow Assassin
             Mountain 22 11, Mountain 23 11, Mountain 24 11, Mountain 25 11, Mountain 27 11, Mountain 28 11, Mountain 29 11,
             Mountain 22 12, Mountain 29 12, Mountain 22 13, Mountain 29 13, Mountain 22 14, Mountain 29 14, Mountain 22 15, Mountain 29 15,
             Mountain 22 16, Mountain 23 16, Mountain 24 16, Mountain 25 16, Mountain 26 16, Mountain 27 16, Mountain 28 16, Mountain 29 16]
,
      -- set it to True to display debug messages
      displayLogs = False,
      inBattle = False,
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