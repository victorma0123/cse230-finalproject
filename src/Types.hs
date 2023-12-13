{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}

module Types where

import Brick (Widget)

-- Types

-- | Game state
data Game = Game
  { posX :: Int,
    posY :: Int,
    shield :: Int,
    sword :: Int,
    hp :: Int,
    attack :: Int,
    events :: [GameEvent],
    iChoice :: Int,
    inEvent :: Maybe GameEvent,
    monsters :: [Monster],
    -- track the position of the monster
    -- that we are facing so that we can remove
    -- it after it is defeated
    inMonster :: Maybe Monster,
    gameOver :: Bool,
    mountains :: [Mountain],
    treasureOpened :: Bool
  }

data GameEvent = GEvent
  { eventX :: Int,
    eventY :: Int,
    name :: String,
    description :: String,
    choices :: [EventChoice],
    icon :: Widget Name
  }

data EventChoice = GChoice
  { title :: String,
    effect :: Game -> Game
  }

data Monster = Monster
  { monsterPosX :: Int,
    monsterPosY :: Int,
    monsterName :: String,
    monsterHp :: Int,
    monsterAttack :: Int
    -- other attributes
  }
  deriving (Eq, Show)

data Mountain = Mountain
  { mountainPosX :: Int,
    mountainPosY :: Int
  }
  deriving (Eq, Show)

-- Types

-- | Ticks mark passing of time
--
-- This is our custom event that will be constantly fed into the app.
data Tick = Tick

-- | Named resources
--
-- Not currently used, but will be easier to refactor
-- if we call this "Name" now.
type Name = ()

data Bonus = NoBonus | HPBonus Int | AttackBonus Int | ShieldBonus Int | SwordBonus Int