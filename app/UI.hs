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
handleEvent g (VtyEvent (V.EvKey (V.KChar 'w') [])) =
  continue $
    g
      { posY = posY g - 1,
        inEvent = getEvent (posX g) (posY g - 1) g,
        iChoice = 0
      }
handleEvent g (VtyEvent (V.EvKey (V.KChar 'a') [])) =
  continue $
    g
      { posX = posX g - 1,
        inEvent = getEvent (posX g - 1) (posY g) g,
        iChoice = 0
      }
handleEvent g (VtyEvent (V.EvKey (V.KChar 's') [])) =
  continue $
    g
      { posY = posY g + 1,
        inEvent = getEvent (posX g) (posY g + 1) g,
        iChoice = 0
      }
handleEvent g (VtyEvent (V.EvKey (V.KChar 'd') [])) =
  continue $
    g
      { posX = posX g + 1,
        inEvent = getEvent (posX g + 1) (posY g) g,
        iChoice = 0
      }
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
handleEvent g _ = continue g

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

getEvent :: Int -> Int -> Game -> Maybe GameEvent
getEvent x y g = go (events g)
  where
    go [] = Nothing
    go (e : es) =
      if (eventX e == x) && (eventY e == y)
        then Just e
        else go es

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
  if (posX g == x) && (posY g == y)
    then str "."
    else case getEvent x y g of
      Nothing -> str " "
      Just e -> icon e
