{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}

module Types where

import Brick (Widget)

-- Types

-- | Game state
data Game = Game
  { posX :: Int,
    posY :: Int,
    sheild :: Int,
    sword :: Int,
    hp :: Int,
    attack :: Int,
    events :: [GameEvent]
  }

data GameEvent = GEvent
  { eventX :: Int,
    eventY :: Int,
    name :: String,
    choices :: [EventChoice],
    icon :: Widget Name
  }

data EventChoice = GChoice
  { title :: String
  -- other effects
  }
  deriving (Show)

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