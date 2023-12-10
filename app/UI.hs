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
import qualified Graphics.Vty as V
import Types
import GameLogic (moveMonster, monsterEncounterEvent)
import Control.Monad.IO.Class (liftIO)
import Data.List (find)

-- global config
-- this is because one column take less space than one row.
-- Multiply this value on column to make it an rough square
gRow2Col :: Int
gRow2Col = 2

gHeight :: Int
gHeight = 40

gWidth :: Int
gWidth = 40

gMapRows :: Int
gMapRows = 20

gMapCols :: Int
gMapCols = 30

gMapHeight :: Int
gMapHeight = 2 * gHeight `div` 3

gBarHeight :: Int
gBarHeight = gHeight - gMapHeight

inf :: Int
inf = 1000000

-- Handling events

handleEvent :: Game -> BrickEvent Name Tick -> EventM Name (Next Game)
handleEvent g (VtyEvent (V.EvKey V.KEnter [])) = continue $
  case inEvent g of
    Nothing -> g
    Just e -> effect (choices e !! iChoice g) g
-- handleEvent g (VtyEvent (V.EvKey (V.KChar 'w') [])) =
--   continue $
--     g
--       { posY = posY g - 1,
--         inEvent = getEvent (posX g) (posY g - 1) g,
--         iChoice = 0
--       }
-- handleEvent g (VtyEvent (V.EvKey (V.KChar 'a') [])) =
--   continue $
--     g
--       { posX = posX g - 1,
--         inEvent = getEvent (posX g - 1) (posY g) g,
--         iChoice = 0
--       }
-- handleEvent g (VtyEvent (V.EvKey (V.KChar 's') [])) =
--   continue $
--     g
--       { posY = posY g + 1,
--         inEvent = getEvent (posX g) (posY g + 1) g,
--         iChoice = 0
--       }
-- handleEvent g (VtyEvent (V.EvKey (V.KChar 'd') [])) =
--   continue $
--     g
--       { posX = posX g + 1,
--         inEvent = getEvent (posX g + 1) (posY g) g,
--         iChoice = 0
--       }
handleEvent g (VtyEvent (V.EvKey (V.KChar 'w') [])) =
  continue $ movePlayer (0, -1) g
handleEvent g (VtyEvent (V.EvKey (V.KChar 'a') [])) =
  continue $ movePlayer (-1, 0) g
handleEvent g (VtyEvent (V.EvKey (V.KChar 's') [])) =
  continue $ movePlayer (0, 1) g
handleEvent g (VtyEvent (V.EvKey (V.KChar 'd') [])) =
  continue $ movePlayer (1, 0) g
handleEvent g (VtyEvent (V.EvKey (V.KChar 'q') [])) = halt g
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
handleEvent g (AppEvent Tick) = do
  let mapWidth = gMapCols
      mapHeight = gMapRows
  -- Move monsters only if they are not in an event with the player
  newMonsters <- liftIO $ mapM (\m -> if isEngagedInEvent m g then return m else moveMonster m mapWidth mapHeight) (monsters g)
  let updatedGame = checkForEncounters g { monsters = newMonsters }
  continue updatedGame
handleEvent g _ = continue g

-- Functions Used in handleEvent

movePlayer :: (Int, Int) -> Game -> Game
movePlayer (dx, dy) game =
  let newX = posX game + dx
      newY = posY game + dy
      maybeMonster = getMonsterAt newX newY game
      maybeEvent = getEvent newX newY game
      newInEvent = case maybeMonster of
                     Just _ -> Just monsterEncounterEvent
                     Nothing -> maybeEvent
  in game { posX = newX, posY = newY, inEvent = newInEvent }


charToChoiceIndex :: Char -> Int
charToChoiceIndex char = fromEnum char - fromEnum '1'

checkForEncounters :: Game -> Game
checkForEncounters game =
  if any (\m -> monsterPosX m == posX game && monsterPosY m == posY game) (monsters game)
  then game { inEvent = Just monsterEncounterEvent }  -- Trigger monster encounter
  else game  -- No changes if no encounters

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

drawMap :: Game -> Widget Name
drawMap g = vBox [createRow y g | y <- [0 .. gMapRows]] -- 生成地图的每一行

drawStatus :: Game -> Widget n
drawStatus g =
  str ("Shield: " ++ show (sheild g))
    <=> str ("Sword: " ++ show (sword g))
    <=> str ("HP: " ++ show (hp g))
    <=> str ("Attack: " ++ show (attack g))

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
              <+> str ("Choice " ++ show i ++ ": " ++ title (choices event !! i))
            | i <- [0 .. length (choices event) - 1]
          ]

-- getEvent :: Int -> Int -> Game -> Maybe GameEvent
-- getEvent x y g = go (events g)
--   where
--     go [] = Nothing
--     go (e : es) =
--       if (eventX e == x) && (eventY e == y)
--         then Just e
--         else go es
getEvent :: Int -> Int -> Game -> Maybe GameEvent
getEvent x y game = find (\e -> eventX e == x && eventY e == y) (events game)


drawUI :: Game -> [Widget Name]
drawUI g =
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

-- 创建地图的一行
createRow :: Int -> Game -> Widget Name
createRow y g =
  let mapCells = [setAvailableSize (gRow2Col, 1) $ center $ createCell x y g | x <- [0 .. gMapCols]] -- 生成一行中的每个格子
   in hBox mapCells

-- 创建单个格子
createCell :: Int -> Int -> Game -> Widget Name
createCell x y g =
  case renderMonster x y g of
    Just monsterWidget -> monsterWidget
    Nothing -> 
      if (posX g == x) && (posY g == y)
        then str "."
        else case getEvent x y g of
          Nothing -> str " "
          Just e -> icon e
