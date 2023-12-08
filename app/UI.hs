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
handleEvent g _ = continue g
-- Drawing

fixedSizeWidget :: Int -> Int -> Widget n
fixedSizeWidget width height = hLimit width $ vLimit height $ fill ' '


-- string
stringposition g = str $ "X Position: " ++ show (posX g) ++ "  Y Position: " ++ show (posY g)

drawUI :: Game -> [Widget Name]
drawUI g =
  let mapRows = [ createRow y g | y <- [0..3] ]  -- 生成地图的每一行
  in [ border $ vBox
        [ fixedSizeWidget 62 12, stringposition g
        , vBox mapRows  -- 将地图行添加到界面中
        , border $ hBox [ border $ vBox 
                    [ fixedSizeWidget 30 3,
                      fixedSizeWidget 30 3 , str $ "sheild: \n" ++ "sword:\n" ++ "hp:\n" ++ "attack:\n"]
                , border $ vBox 
                    [fixedSizeWidget 30 3,
                      fixedSizeWidget 30 3]
                ]
        ]
     ]

-- 创建地图的一行
createRow :: Int -> Game -> Widget Name
createRow y g =
  let mapCells = [ createCell x y g | x <- [0..3] ]  -- 生成一行中的每个格子
  in hBox mapCells

-- 创建单个格子
createCell :: Int -> Int -> Game -> Widget Name
createCell x y g =
  let content = if x == posX g && y == posY g then "玩家" else "  "  -- 根据游戏状态决定格子内容
  in str content