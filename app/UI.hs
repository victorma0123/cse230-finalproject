{-# LANGUAGE OverloadedStrings #-}

module UI where

import Brick
  ( BrickEvent (..),
    EventM,
    Next,
    Padding (Max, Pad),
    Widget,
    continue,
    fill,
    hBox,
    hLimit,
    halt,
    joinBorders,
    str,
    vBox,
    vLimit,
    (<+>),
    (<=>),
  )
import Brick.Widgets.Border
import Brick.Widgets.Border.Style
import Brick.Widgets.Center
import Brick.Widgets.Core
import Control.Monad.IO.Class (liftIO)
import Data.List (find)
import GameLogic (monsterEncounterEvent, moveMonster)
import qualified Graphics.Vty as V
import Init
import Types

-- global config
-- this is because one column take less space than one row.
-- Multiply this value on column to make it an rough square

gRow2Col :: Int
gRow2Col = 2

gWidth :: Int
gWidth = 40

gMapRows :: Int
gMapRows = 26

gMapCols :: Int
gMapCols = 36

gMapHeight :: Int
gMapHeight = 20

gBarHeight :: Int
gBarHeight = 6

inf :: Int
inf = 1000000

-- Handling events

handleEvent :: Game -> BrickEvent Name Tick -> EventM Name (Next Game)
handleEvent g (VtyEvent (V.EvKey V.KEnter [])) = continue $
  case inEvent g of
    Nothing -> g
    Just e -> effect (choices e !! iChoice g) g
-- Handle for moving
handleEvent g (VtyEvent (V.EvKey (V.KChar 'w') [])) =
  continue $ movePlayer (0, -1) g
handleEvent g (VtyEvent (V.EvKey (V.KChar 'a') [])) =
  continue $ movePlayer (-1, 0) g
handleEvent g (VtyEvent (V.EvKey (V.KChar 's') [])) =
  continue $ movePlayer (0, 1) g
handleEvent g (VtyEvent (V.EvKey (V.KChar 'd') [])) =
  continue $ movePlayer (1, 0) g
-- Handle for quit game
handleEvent g (VtyEvent (V.EvKey (V.KChar 'q') [])) = halt g
-- Handle make choice in event
handleEvent g (VtyEvent (V.EvKey V.KUp [])) = continue $ g {iChoice = max (iChoice g - 1) 0}
handleEvent g (VtyEvent (V.EvKey V.KDown [])) =
  continue $
    g
      { iChoice =
          case inEvent g of
            Nothing -> 0
            Just e -> min (iChoice g + 1) (length (choices e) - 1)
      }
handleEvent g (VtyEvent (V.EvKey (V.KChar char) [])) = continue $
  case inEvent g of
    Just event ->
      let choiceIndex = charToChoiceIndex char
       in if choiceIndex >= 0 && choiceIndex < length (choices event)
            then effect (choices event !! choiceIndex) g
            else g
    Nothing -> g
-- Locking monster when meeting with player
handleEvent g (AppEvent Tick) = do
  let mapWidth = gMapCols
      mapHeight = gMapRows
  -- Move monsters only if they are not in an event with the player
  newMonsters <- liftIO $ mapM (\m -> if isEngagedInEvent m g then return m else moveMonster m mapWidth mapHeight) (monsters g)
  let updatedGame = checkForEncounters g {monsters = newMonsters}
  continue updatedGame
handleEvent g _ = continue g

-- Functions Used in handleEvent

-- Determine the hp
checkGameOver :: Game -> EventM Name (Next Game)
checkGameOver game =
  if hp game <= 0 then halt game else continue game

-- Determine if meeting with one of the monsters
isMonsterEncounter :: Game -> Bool
isMonsterEncounter game =
  any (\m -> monsterPosX m == posX game && monsterPosY m == posY game) (monsters game)

-- Helper function to handle player movement
movePlayerHelper :: (Int, Int) -> Game -> Game
movePlayerHelper (dx, dy) game =
  let newX = posX game + dx
      newY = posY game + dy
      maybeMonster = getMonsterAt newX newY game
      maybeEvent = getEvent newX newY game
      newInEvent = case maybeMonster of
        Just _ -> Just monsterEncounterEvent
        Nothing -> maybeEvent
   in -- we can always safely set choice index to 0 when we move to a new cell
      game {posX = newX, posY = newY, inEvent = newInEvent, iChoice = 0, inMonster = maybeMonster}

movePlayer :: (Int, Int) -> Game -> Game
movePlayer (dx, dy) game =
  let newX = posX game + dx
      newY = posY game + dy
      isMountain = any (\m -> mountainPosX m == newX && mountainPosY m == newY) (mountains game)
   in if isMountain
        then game -- 如果新位置有山脉，则不移动玩家
        else movePlayerHelper (dx, dy) game

charToChoiceIndex :: Char -> Int
charToChoiceIndex char = fromEnum char - fromEnum '1'

checkForEncounters :: Game -> Game
checkForEncounters game =
  if any (\m -> monsterPosX m == posX game && monsterPosY m == posY game) (monsters game)
    then game {inEvent = Just monsterEncounterEvent} -- Trigger monster encounter
    else game -- No changes if no encounters

isEngagedInEvent :: Monster -> Game -> Bool
isEngagedInEvent monster game =
  monsterPosX monster == posX game && monsterPosY monster == posY game

renderMonster :: Int -> Int -> Game -> Maybe (Widget Name)
renderMonster x y game =
  if isMonsterAt x y game
    then Just $ str "M"
    else Nothing

isMonsterAt :: Int -> Int -> Game -> Bool
isMonsterAt x y game = any (\m -> monsterPosX m == x && monsterPosY m == y) (monsters game)

getMonsterAt :: Int -> Int -> Game -> Maybe Monster
getMonsterAt x y game = find (\m -> monsterPosX m == x && monsterPosY m == y) (monsters game)

-- Drawing
-- getEvent :: Int -> Int -> Game -> Maybe GameEvent
-- getEvent x y g = go (events g)
--   where
--     go [] = Nothing
--     go (e : es) =
--       if (eventX e == x) && (eventY e == y)
--         then Just e
--         else go es

-- Generate the interface
drawUI :: Game -> [Widget Name]
drawUI g =
  if gameOver g
    then [drawGameOverScreen]
    else
      let mapRows = drawMap g
       in [ joinBorders $
              border $
                hLimit (gWidth * gRow2Col) $
                  vBox
                    [ setAvailableSize (gWidth * gRow2Col, gMapHeight) $ center $ border mapRows, -- 将地图行添加到界面中
                      hBorder,
                      setAvailableSize (gWidth * gRow2Col, gBarHeight) $
                        hBox
                          [ hLimit (gWidth * gRow2Col `div` 2) $ vCenter $ padRight Max $ drawStatus g,
                            vBorder,
                            hLimit (gWidth * gRow2Col `div` 2) $ vCenter $ padRight Max $ drawEvent g
                          ]
                    ]
          ]

-- Game Over
drawGameOverScreen :: Widget Name
drawGameOverScreen =
  center $
    borderWithLabel (str "Game Over") $
      ( padAll 1 $
          vBox
            [ str "Game Over!",
              str " ",
              str "Press q to exit."
            ]
      )

-- Create the map
drawMap :: Game -> Widget Name
drawMap g = vBox [createRow y g | y <- [0 .. gMapRows]]

-- Create the row in map
createRow :: Int -> Game -> Widget Name
createRow y g =
  let mapCells = [setAvailableSize (gRow2Col, 1) $ center $ createCell x y g | x <- [0 .. gMapCols]] -- 生成一行中的每个格子
   in hBox mapCells

-- Create cells in map
createCell :: Int -> Int -> Game -> Widget Name
createCell x y g =
  case renderMountain x y g of
    Just mountainWidget -> mountainWidget
    Nothing ->
      case renderMonster x y g of
        Just monsterWidget -> monsterWidget
        Nothing ->
          if (posX g == x) && (posY g == y)
            then str "." -- 用 "." 表示玩家
            else case getEvent x y g of
              Nothing -> str " " -- 空白表示空单元格
              Just e -> icon e -- 用事件的图标表示事件

-- Status Bar
drawStatus :: Game -> Widget n
drawStatus g =
  str ("Press 'q' to quit the game.\n\n")
    <=> str ("Shield: " ++ show (sheild g))
    <=> str ("Sword: " ++ show (sword g))
    <=> str ("HP: " ++ show (hp g))
    <=> str ("Attack: " ++ show (attack g))

-- Get current event
getEvent :: Int -> Int -> Game -> Maybe GameEvent
getEvent x y game = find (\e -> eventX e == x && eventY e == y) (events game)

renderMountain :: Int -> Int -> Game -> Maybe (Widget Name)
renderMountain x y game =
  if isMountainAt x y game
    then Just $ str "⛰" -- 用 "⛰" 表示山脉
    else Nothing

isMountainAt :: Int -> Int -> Game -> Bool
isMountainAt x y game = any (\m -> mountainPosX m == x && mountainPosY m == y) (mountains game)

-- Event Bar
drawEvent :: Game -> Widget n
drawEvent g =
  case inEvent g of
    Nothing -> str ""
    (Just event) ->
      str ("Event: " ++ name event)
        <=> str (description event)
        <=> vBox
          [ ( if i == iChoice g
                || (iChoice g < 0 && i == 0)
                || (iChoice g >= length (choices event) && i == length (choices event) - 1)
                then str "> "
                else emptyWidget
            )
              <+> str ("Choice " ++ show (i + 1) ++ ": " ++ title (choices event !! i))
            | i <- [0 .. length (choices event) - 1]
          ]
