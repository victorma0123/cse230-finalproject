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
import Types (Game, Name, Tick)

-- Handling events

handleEvent :: Game -> BrickEvent Name Tick -> EventM Name (Next Game)
handleEvent g (VtyEvent (V.EvKey V.KEnter [])) = halt g
handleEvent g _ = continue g

-- Drawing

fixedSizeWidget :: Int -> Int -> Widget n
fixedSizeWidget width height = hLimit width $ vLimit height $ fill ' '

drawUI :: Game -> [Widget Name]
drawUI g =
  [ border $ vBox
      [ fixedSizeWidget 62 12
      , border $ hBox [ border $ vBox 
                  [ fixedSizeWidget 30 3,
                     fixedSizeWidget 30 3]
              , border $ vBox 
                  [fixedSizeWidget 30 3,
                     fixedSizeWidget 30 3]
              ]
      ]
  ]