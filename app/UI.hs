{-# LANGUAGE OverloadedStrings #-}

module UI where

import Brick
  ( BrickEvent (..),
    EventM,
    Next,
    Widget,
    continue,
    halt,
    str,
    vBox,
    hBox,
    vLimit,
    hLimit,
    fill,
  )
import Brick.Widgets.Border
import Brick.Widgets.Border.Style
import qualified Graphics.Vty as V
import Types (Game(..), Name, Tick)


-- Handling events

handleEvent :: Game -> BrickEvent Name Tick -> EventM Name (Next Game)
handleEvent g (VtyEvent (V.EvKey V.KEnter [])) = halt g
handleEvent g (VtyEvent (V.EvKey (V.KChar 'w') [])) = continue $ g { posY = posY g - 1 }
handleEvent g (VtyEvent (V.EvKey (V.KChar 'a') [])) = continue $ g { posX = posX g - 1 }
handleEvent g (VtyEvent (V.EvKey (V.KChar 's') [])) = continue $ g { posY = posY g + 1 }
handleEvent g (VtyEvent (V.EvKey (V.KChar 'd') [])) = continue $ g { posX = posX g + 1 }
handleEvent g (VtyEvent (V.EvKey (V.KChar 'q') [])) = halt g
handleEvent g _ = continue g
-- Drawing

fixedSizeWidget :: Int -> Int -> Widget n
fixedSizeWidget width height = hLimit width $ vLimit height $ fill ' '


-- string
stringposition g = str $ "Press q to exit\n" ++ "X Position: " ++ show (posX g) ++ "  Y Position: " ++ show (posY g)

drawUI :: Game -> [Widget Name]
drawUI g =
  let mapRows = [ createRow y g | y <- [0..10] ]  -- 生成地图的每一行
  in [ border $ vBox
        [stringposition g
        , vBox mapRows  -- 将地图行添加到界面中
        , border $ hBox [ border $ vBox 
                    [ str $ "sheild:\n" ++ "sword:\n" ++ "hp:\n" ++ "attack:\n",
                      fixedSizeWidget 30 1 ]
                , border $ vBox 
                    [ str $ "Event:\n" ++ "Choice 1:\n" ++ "Choice 2: \n",
                      fixedSizeWidget 30 2]
                ]
        ]
     ]

-- 创建地图的一行
createRow :: Int -> Game -> Widget Name
createRow y g =
  let mapCells = [ createCell x y g | x <- [0..30] ]  -- 生成一行中的每个格子
  in hBox mapCells

-- 创建单个格子
createCell :: Int -> Int -> Game -> Widget Name
createCell x y g =
  let content = if x == posX g && y == posY g then "Player" else "  "  -- 根据游戏状态决定格子内容
  in str content