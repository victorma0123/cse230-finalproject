{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}

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
      eventsMap = fromList [((0, 0), map (\e -> e {isused = False}) initialEvents)],
      iChoice = -1,
      inEvent = Nothing,
      monstersMap =
        fromList
          [ ( (0, 0),
              [Monster 32 9 "Goblin Raider" 30 8, Monster 19 13 "Forest Nymph" 20 6, Monster 2 9 "Mountain Troll" 50 10, Monster 19 3 "Shadow Assassin" 40 12]
            )
          ],
      inMonster = Nothing,
      gameOver = False,
      mountainsMap =
        fromList
          [ ( (0, 0),
              generateSpiralMountains 17 37
            )
          ],
      winner = False,
      loser = False,
      -- set it to True to display debug messages
      displayLogs = False,
      inBattle = False,
      logs = [],
      finalMonsterHp = 150,
      finalMonsterAttack = 20
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
    ancientLibrary,
    finalConfrontation
  ]

generateSpiralMountains :: Int -> Int -> [Mountain]
generateSpiralMountains rows cols = [Mountain 5 3, Mountain 5 4, Mountain 5 5] ++ generateSpiralMountainsHelper 1 0 rows cols False

generateSpiralMountainsHelper :: Int -> Int -> Int -> Int -> Bool -> [Mountain]
generateSpiralMountainsHelper startX startY height width changeDirection
  | height <= 1 || width <= 1 = []
  | otherwise =
      if changeDirection
      then [Mountain x startY | x <- [(startX + 2)..startX + width - 1]] ++ 
           [Mountain (startX + width - 1) y | y <- [(startY)..startY + height]] ++ 
           generateSpiralMountainsHelper (startX + 2) (startY + 2) (height - 4) (width - 4) False
      else [Mountain startX y | y <- [(startY + 2)..startY + height - 1]] ++ 
           [Mountain x (startY + height - 1) | x <- [(startX + 1)..startX + width - 3]] ++
           generateSpiralMountainsHelper (startX + 2) (startY + 2) (height - 4) (width - 4) True
