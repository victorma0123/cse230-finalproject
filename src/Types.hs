{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}

module Types where

-- Types

-- | Game state
data Game = Game
  { posX :: Int,
    posY :: Int,
    sheild :: Int,
    sword :: Int,
    hp :: Int,
    attack :: Int
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