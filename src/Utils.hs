{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}

module Utils where

import Brick
import Control.Monad.IO.Class (liftIO)
import Data.List (nub, nubBy)
import Data.Map
import Debug
import Events
import System.Random (randomRIO)
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
  Nothing -> []
  Just evts -> evts

getMapRegionMountains :: (Int, Int) -> Game -> [Mountain]
getMapRegionMountains (x, y) g = case lookup (getMapRegionCoord (x, y)) (mountainsMap g) of
  Nothing -> []
  Just mountains -> mountains

getMapRegionMonsters :: (Int, Int) -> Game -> [Monster]
getMapRegionMonsters (x, y) g = case lookup (getMapRegionCoord (x, y)) (monstersMap g) of
  Nothing -> []
  Just monsters -> monsters

getCurrentRegionEvents :: Game -> [GameEvent]
getCurrentRegionEvents g = getMapRegionEvents (posX g, posY g) g

getCurrentRegionMountains :: Game -> [Mountain]
getCurrentRegionMountains g = getMapRegionMountains (posX g, posY g) g

getCurrentRegionMonsters :: Game -> [Monster]
getCurrentRegionMonsters g = getMapRegionMonsters (posX g, posY g) g

genMapRegionIfNotExist :: Game -> EventM Name (Next Game)
genMapRegionIfNotExist g = case lookup (getMapRegionCoord (posX g, posY g)) (eventsMap g) of
  Just _ -> continue g
  Nothing -> do
    (newEvents, newMountains, newMonsters) <- liftIO genMapRegion
    let (xcoord, ycoord) = getMapRegionCoord (posX g, posY g)
        events' =
          Prelude.map
            ( \evt ->
                evt
                  { eventX = eventX evt + xcoord * gMapCols,
                    eventY = eventY evt + ycoord * gMapRows
                  }
            )
            newEvents
        mounts' =
          Prelude.map
            ( \mnt ->
                mnt
                  { mountainPosX = mountainPosX mnt + xcoord * gMapCols,
                    mountainPosY = mountainPosY mnt + ycoord * gMapRows
                  }
            )
            newMountains
        mounts'' = Prelude.filter (\mnt -> mountainPosX mnt /= posX g || mountainPosY mnt /= posY g) mounts'
        mons' =
          Prelude.map
            ( \mon ->
                mon
                  { monsterPosX = monsterPosX mon + xcoord * gMapCols,
                    monsterPosY = monsterPosY mon + ycoord * gMapRows
                  }
            )
            newMonsters
        msg = show $ getMapRegionCoord (posX g, posY g)
     in continue $
          appendKeyValueLogToGame
            "gen"
            msg
            g
              { eventsMap = insert (getMapRegionCoord (posX g, posY g)) events' (eventsMap g),
                mountainsMap = insert (getMapRegionCoord (posX g, posY g)) mounts'' (mountainsMap g),
                monstersMap = insert (getMapRegionCoord (posX g, posY g)) mons' (monstersMap g)
              }

genMapRegion :: IO ([GameEvent], [Mountain], [Monster])
genMapRegion = do
  mounts <- liftIO genMountains
  evts <- liftIO genEvents
  mons <- liftIO genMonsters
  -- remove overlapped events and monsters
  let evts' = removeOverlappedEvents evts mounts
      mons' = removeOverlappedMonsters mons mounts
   in return (evts', mounts, mons')

genMountains :: IO [Mountain]
genMountains = do
  n <- randomRIO (4, 10)
  -- generate n mountains
  mounts <- sequence $ replicate n genMountain
  return $ collectMountains mounts

genMountain :: IO [Mountain]
genMountain = do
  x <- randomRIO (0, gMapCols - 1) :: IO Int
  y <- randomRIO (0, gMapRows - 1) :: IO Int
  p <- randomRIO (0.5, 0.8) :: IO Double
  go x y p [Mountain x y]
  where
    go :: Int -> Int -> Double -> [Mountain] -> IO [Mountain]
    go x y p ms = do
      p' <- randomRIO (0.0, 1.0) :: IO Double
      if p' > p
        then return ms
        else do
          dir <- randomRIO (0, 3) :: IO Int
          let (x', y') = case dir of
                0 -> (x + 1, y)
                1 -> (x - 1, y)
                2 -> (x, y + 1)
                3 -> (x, y - 1)
           in go x' y' p (Mountain x' y' : ms)

collectMountains :: [[Mountain]] -> [Mountain]
collectMountains mountss =
  Prelude.filter
    ( \(Mountain x y) ->
        x >= 0
          && x < gMapCols
          && y >= 0
          && y < gMapRows
    )
    $ nub
    $ concat mountss

genEvents :: IO [GameEvent]
genEvents = do
  n <- randomRIO (3, 7)
  -- generate n events
  evts <- sequence $ replicate n genEvent
  return $
    nubBy
      ( \e1 e2 ->
          eventX e1 == eventX e2
            && eventY e1 == eventY e2
      )
      evts

genEvent :: IO GameEvent
genEvent = do
  -- randomly select an event from allEvents list
  i <- randomRIO (0, length allEvents - 1) :: IO Int
  x <- randomRIO (0, gMapCols - 1) :: IO Int
  y <- randomRIO (0, gMapRows - 1) :: IO Int
  let evt = allEvents !! i
   in return $ evt {eventX = x, eventY = y}

genMonsters :: IO [Monster]
genMonsters = do
  n <- randomRIO (2, 5)
  -- generate n monsters
  mons <- sequence $ replicate n genMonster
  return mons

genMonster :: IO Monster
genMonster = do
  -- randomly select a monster from allMonsters list
  i <- randomRIO (0, length allMonsters - 1) :: IO Int
  x <- randomRIO (0, gMapCols - 1) :: IO Int
  y <- randomRIO (0, gMapRows - 1) :: IO Int
  let mon = allMonsters !! i
   in return $ mon {monsterPosX = x, monsterPosY = y}

removeOverlappedEvents :: [GameEvent] -> [Mountain] -> [GameEvent]
removeOverlappedEvents evts mounts =
  Prelude.filter
    ( \evt ->
        not $
          Prelude.any
            ( \mnt ->
                eventX evt == mountainPosX mnt
                  && eventY evt == mountainPosY mnt
            )
            mounts
    )
    evts

removeOverlappedMonsters :: [Monster] -> [Mountain] -> [Monster]
removeOverlappedMonsters mons mounts =
  Prelude.filter
    ( \mon ->
        not $
          Prelude.any
            ( \mnt ->
                monsterPosX mon == mountainPosX mnt
                  && monsterPosY mon == mountainPosY mnt
            )
            mounts
    )
    mons