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
import Types (Game (..), Name, Tick)

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
handleEvent g (VtyEvent (V.EvKey V.KEnter [])) = halt g
handleEvent g (VtyEvent (V.EvKey (V.KChar 'w') [])) = continue $ g {posY = posY g - 1}
handleEvent g (VtyEvent (V.EvKey (V.KChar 'a') [])) = continue $ g {posX = posX g - 1}
handleEvent g (VtyEvent (V.EvKey (V.KChar 's') [])) = continue $ g {posY = posY g + 1}
handleEvent g (VtyEvent (V.EvKey (V.KChar 'd') [])) = continue $ g {posX = posX g + 1}
handleEvent g (VtyEvent (V.EvKey (V.KChar 'q') [])) = halt g
handleEvent g _ = continue g

-- Drawing

-- string
stringposition g = str $ "Press q to exit\n" ++ "X Position: " ++ show (posX g) ++ "  Y Position: " ++ show (posY g)

setBoxSize :: Int -> Int -> Widget n -> Widget n
setBoxSize w h wg = hLimit w $ vLimit h wg

drawMap :: Game -> Widget Name
drawMap g = vBox [createRow y g | y <- [0 .. gMapRows]] -- 生成地图的每一行

drawStatus :: Game -> Widget n
drawStatus g =
  str ("Shield: " ++ show (sheild g))
    <=> str ("Sword: " ++ show (sword g))
    <=> str ("HP: " ++ show (hp g))
    <=> str ("Attack: " ++ show (attack g))

drawEvent :: p -> Widget n
drawEvent g =
  str ("Event: " ++ "some event")
    <=> str ("Choice 1: " ++ "some choice")
    <=> str ("Choice 2: " ++ "some other choice")

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
  let mapCells = [createCell x y g | x <- [0 .. gMapCols]] -- 生成一行中的每个格子
   in hBox mapCells

-- 创建单个格子
createCell :: Int -> Int -> Game -> Widget Name
createCell x y g =
  let content = if x == posX g && y == posY g then "Player" else "  " -- 根据游戏状态决定格子内容
   in str content
