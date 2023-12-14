{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}

module Utils where

import Data.Map
import Types
import Prelude hiding (lookup)

-- global config
-- this is because one column take less space than one row.
-- Multiply this value on column to make it an rough square

gRow2Col :: Int
gRow2Col = 2

gWidth :: Int
gWidth = 40

gMapRows :: Int
gMapRows = 18

gMapCols :: Int
gMapCols = 38

gMapHeight :: Int
gMapHeight = 20

gBarHeight :: Int
gBarHeight = 6

inf :: Int
inf = 1000000

getMapRegionCoord :: (Int, Int) -> (Int, Int)
getMapRegionCoord (x, y) = (x `div` gMapCols, y `div` gMapRows)

getMapRegionEvents :: (Int, Int) -> Game -> [GameEvent]
getMapRegionEvents (x, y) g = case lookup (getMapRegionCoord (x, y)) (eventsMap g) of
  Nothing -> error "this region of map does not exist"
  Just evts -> evts

getMapRegionMountains :: (Int, Int) -> Game -> [Mountain]
getMapRegionMountains (x, y) g = case lookup (getMapRegionCoord (x, y)) (mountainsMap g) of
  Nothing -> error "this region of map does not exist"
  Just mountains -> mountains

getMapRegionMonsters :: (Int, Int) -> Game -> [Monster]
getMapRegionMonsters (x, y) g = case lookup (getMapRegionCoord (x, y)) (monstersMap g) of
  Nothing -> error "this region of map does not exist"
  Just monsters -> monsters

getCurrentRegionEvents :: Game -> [GameEvent]
getCurrentRegionEvents g = getMapRegionEvents (posX g, posY g) g

getCurrentRegionMountains :: Game -> [Mountain]
getCurrentRegionMountains g = getMapRegionMountains (posX g, posY g) g

getCurrentRegionMonsters :: Game -> [Monster]
getCurrentRegionMonsters g = getMapRegionMonsters (posX g, posY g) g