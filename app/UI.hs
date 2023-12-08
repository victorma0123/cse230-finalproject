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

drawUI :: Game -> [Widget Name]
drawUI g =
  [ border $ vBox
      [ fixedSizeWidget 62 12, str $ "X Position: " ++ show (posX g) ++ "Y Position: " ++ show (posY g)
      , border $ hBox [ border $ vBox 
                  [ fixedSizeWidget 30 3,
                     fixedSizeWidget 30 3 , str $ "sheild: \n" ++ "sword:"]
              , border $ vBox 
                  [fixedSizeWidget 30 3,
                     fixedSizeWidget 30 3]
              ]
      ]
  ]