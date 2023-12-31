{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}

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
import Brick.AttrMap (AttrMap, AttrName, attrMap, attrName)
import Brick.Util (fg, on)
import Brick.Widgets.Border
import Brick.Widgets.Border.Style
import Brick.Widgets.Center
import Brick.Widgets.Core
import Control.Monad.IO.Class (liftIO)
import Data.List (find)
import Data.Map (Map, insert)
import Debug
import GameLogic
import qualified Graphics.Vty as V
import Init
import Types
import Utils

-- 定义绿色属性
greenAttr :: AttrName
greenAttr = attrName "green"

-- 在 attrMap 中添加您的新属性
theMap :: AttrMap
theMap =
  attrMap
    V.defAttr
    [(greenAttr, fg V.green)]

markEventAsUsed :: GameEvent -> Game -> Map (Int, Int) [GameEvent]
markEventAsUsed evt g = insert key newEvents (eventsMap g)
  where
    key = getMapRegionCoord (eventX evt, eventY evt)
    newEvents =
      map
        ( \e ->
            if shouldMarkAsUsed e
              then e {isused = True}
              else e
        )
        (getCurrentRegionEvents g)
    shouldMarkAsUsed e =
      name e == name evt &&
      eventX e == eventX evt &&
      eventY e == eventY evt &&
      not (isFinalConfrontation e)

    isFinalConfrontation e = name e == "Final Confrontation: The Dark Overlord's Lair"


-- Handling events

handleEvent :: Game -> BrickEvent Name Tick -> EventM Name (Next Game)
handleEvent g (VtyEvent (V.EvKey V.KEnter [])) = continue $
  case inEvent g of
    Nothing -> g
    Just e ->
      let newGame = updateGameState $ effect (choices e !! iChoice g) g
          updatedEventsMap = if isFinalConfrontation e
                             then eventsMap g  -- 如果是 finalConfrontation，不更新 eventsMap
                             else markEventAsUsed e newGame
       in newGame
            { eventsMap = updatedEventsMap,
              inEvent = case inMonster newGame of
                Nothing -> Nothing
                Just _ -> inEvent newGame
            }
-- Handle for moving
handleEvent g (VtyEvent (V.EvKey (V.KChar 'w') [])) =
  genMapRegionIfNotExist $ moveAndUpdateBattleState (0, -1) g
handleEvent g (VtyEvent (V.EvKey (V.KChar 'a') [])) =
  genMapRegionIfNotExist $ moveAndUpdateBattleState (-1, 0) g
handleEvent g (VtyEvent (V.EvKey (V.KChar 's') [])) =
  genMapRegionIfNotExist $ moveAndUpdateBattleState (0, 1) g
handleEvent g (VtyEvent (V.EvKey (V.KChar 'd') [])) =
  genMapRegionIfNotExist $ moveAndUpdateBattleState (1, 0) g
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
  -- 移动怪物，只有当他们不在与玩家的事件中时
  newMonsters <-
    liftIO $
      mapM
        ( \m ->
            if isEngagedInEvent m g
              then return m
              else moveMonster m g
        )
        (getCurrentRegionMonsters g)

  -- 更新游戏状态：怪物位置和动画帧
  let updatedGame = g
        { animationFrame = (animationFrame g + 1) `mod` totalFrames,
          monstersMap = insert (getMapRegionCoord (posX g, posY g)) newMonsters (monstersMap g)
        }
  
  continue $ checkForEncounters updatedGame
  where
    totalFrames = 4  -- 动画总帧数
handleEvent g _ = continue g

-- Helper function to move player and update battle state
moveAndUpdateBattleState :: (Int, Int) -> Game -> Game
moveAndUpdateBattleState move g =
  let updatedGame = movePlayer move g
      playerPos = (posX updatedGame, posY updatedGame)
      monsterPos = map (\m -> (monsterPosX m, monsterPosY m)) (getCurrentRegionMonsters updatedGame)
   in if any (== playerPos) monsterPos
        then updatedGame {inBattle = True} -- Enter battle state
        else updatedGame

-- Functions Used in handleEvent

-- Determine the hp
checkGameOver :: Game -> EventM Name (Next Game)
checkGameOver game =
  if hp game <= 0 then halt game else continue game

-- Determine if meeting with one of the monsters
isMonsterEncounter :: Game -> Bool
isMonsterEncounter game =
  any (\m -> monsterPosX m == posX game && monsterPosY m == posY game) (getCurrentRegionMonsters game)

-- Helper function to handle player movement
movePlayerHelper :: (Int, Int) -> Game -> Game
movePlayerHelper (dx, dy) game =
  let newX = posX game + dx
      newY = posY game + dy
      maybeMonster = getMonsterAt newX newY game
      maybeEvent = getEvent newX newY game
      newInEvent = case maybeMonster of
        Just m -> Just $ getMonsterEvent (monsterName m)
        Nothing -> maybeEvent
   in -- we can always safely set choice index to 0 when we move to a new cell
      game {posX = newX, posY = newY, inEvent = newInEvent, iChoice = 0, inMonster = maybeMonster}

movePlayerMountain :: (Int, Int) -> Game -> Game
movePlayerMountain (dx, dy) game =
  let newX = posX game + dx
      newY = posY game + dy
      isMountain = any (\m -> mountainPosX m == newX && mountainPosY m == newY) (getMapRegionMountains (newX, newY) game)
   in if isMountain
        then game -- 如果新位置有山脉，则不移动玩家
        else movePlayerHelper (dx, dy) game

movePlayer :: (Int, Int) -> Game -> Game
movePlayer (dx, dy) game =
  case inEvent game of
    Just event ->
      if isMonsterEncounter game
        then game -- Prevent movement in a monster encounter
        else movePlayerMountain (dx, dy) game
    Nothing -> movePlayerMountain (dx, dy) game

charToChoiceIndex :: Char -> Int
charToChoiceIndex char = fromEnum char - fromEnum '1'

checkForEncounters :: Game -> Game
checkForEncounters game =
  case find (\m -> monsterPosX m == posX game && monsterPosY m == posY game) (getCurrentRegionMonsters game) of
    Just monster -> game {inEvent = Just (getMonsterEvent (monsterName monster))}
    Nothing -> game -- No changes if no encounters

getMonsterEvent :: String -> GameEvent
getMonsterEvent monsterName =
  case monsterName of
    "Goblin Raider" -> goblinRaiderEvent
    "Forest Nymph" -> forestNymphEvent
    "Mountain Troll" -> mountainTrollEvent
    "Shadow Assassin" -> shadowAssassinEvent

isEngagedInEvent :: Monster -> Game -> Bool
isEngagedInEvent monster game =
  monsterPosX monster == posX game && monsterPosY monster == posY game

renderMonster :: Int -> Int -> Game -> Maybe (Widget Name)
renderMonster x y game =
  case getMonsterAt x y game of
    Just monster -> Just $ str $ monsterIcon (monsterName monster)
    Nothing -> Nothing

monsterIcon :: String -> String
monsterIcon name = case name of
  "Goblin Raider" -> "🀀" -- Example icon for Goblin Raider
  "Forest Nymph" -> "🀁" -- Example icon for Forest Nymph
  "Mountain Troll" -> "🀂" -- Example icon for Mountain Troll
  "Shadow Assassin" -> "🀃" -- Example icon for Shadow Assassin
  _ -> "🀅" -- Default icon for other monsters

isMonsterAt :: Int -> Int -> Game -> Bool
isMonsterAt x y game = any (\m -> monsterPosX m == x && monsterPosY m == y) (getCurrentRegionMonsters game)

getMonsterAt :: Int -> Int -> Game -> Maybe Monster
getMonsterAt x y game = find (\m -> monsterPosX m == x && monsterPosY m == y) (getCurrentRegionMonsters game)

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
drawUI g
  | winner g = [drawWinnerScreen (animationFrame g)] -- Show winning screen if winner is True
  | loser g = [drawLoserScreen] -- Show losing screen if loser is True
  | gameOver g = [drawGameOverScreen]
  | inBattle g = drawBattleScreen g -- 如果在战斗中，显示战斗界面
  | otherwise =
      let mapRows = drawMap g
       in [ joinBorders $
              border $
                hLimit (gWidth * gRow2Col) $
                  vBox
                    [ setAvailableSize (gWidth * gRow2Col, gMapHeight) $ center $ border mapRows,
                      hBorder,
                      setAvailableSize (gWidth * gRow2Col, gBarHeight) $
                        hBox
                          [ hLimit (gWidth * gRow2Col `div` 2) $ vCenter $ padRight Max $ drawStatus g,
                            vBorder,
                            hLimit (gWidth * gRow2Col `div` 2) $ vCenter $ padRight Max $ drawEvent g
                          ]
                    ]
          ]

drawWinnerScreen :: Int -> Widget Name
drawWinnerScreen frame =
  center $
    withBorderStyle unicodeBold $
      borderWithLabel (str " Victory! ") $
        vBox
          [ str $ chooseString frame
            -- ... 其他内容 ...
       ]

chooseString :: Int -> String
chooseString frame = case frame `mod` 4 of
  0 -> ascstring1
  1 -> ascstring2
  2 -> ascstring3
  3 -> ascstring4
  _ -> "string99"

drawLoserScreen :: Widget Name
drawLoserScreen =
  center $
    withBorderStyle unicodeBold $
      borderWithLabel (str " Defeat ") $
        vBox
          [ str "       .-.        ",
            str "     (o o) boo!   ",
            str "     | O \\        ",
            str "      \\   \\       ",
            str "       `~~~'      ",
            str "   You've been    ",
            str "     defeated.    ",
            str " Press 'q' to exit."
          ]

-- Game Over
drawGameOverScreen :: Widget Name
drawGameOverScreen =
  center $
    borderWithLabel (str "                                Game Over!                      ") $
      ( padAll 1 $
          vBox
            [ str "                                ,;~' Press q to exit'~;,                             ",
              str "                              ,;                     ;,                           ",
              str "                             ;                         ;                          ",
              str "                            ,'                         ',                         ",
              str "                           ,;                           ;,                        ",
              str "                          ; ;      .           .      ; ;                        ",
              str "                          | ;   ______       ______   ; |                        ",
              str "                          |  `/~\"     ~\" . \"~     \"~\\'  |                        ",
              str "                          |  ~  ,-~~~^~, | ,~^~~~-,  ~  |                        ",
              str "                           |   |        }:{        |   |                         ",
              str "                           |   l       / | \\       !   |                         ",
              str "                           .~  (__,.--\" .^. \"--.,__)  ~.                         ",
              str "                           |     ---;' / | \\ `;---     |                         ",
              str "                            \\__.       \\/^\\/       .__/                          ",
              str "                             V| \\                 / |V                           ",
              str "       __                  | |T~\\___!___!___/~T| |                  _____     ",
              str "    .-~  ~\"-.              | |`IIII_I_I_I_IIII'| |               .-~     \"-.  ",
              str "   /         \\             | |\\,III I I I III,/| |              /           Y ",
              str "  Y          ;              \\   `~~~~~~~~~~'    /               i           | ",
              str "  `.   _     `._              \\   .       .   /               __)         .'  ",
              str "    )=~         `-.._           \\  .`/ \\ '.  /           _..-'~         ~\"<_   ",
              str " .-~                 ~`-.._       ^~~~^~~~^       _..-'~                   ~. ",
              str "/                          ~`-.._           _..-'~                           Y",
              str "{        .~\"-.                  ~`-.._ .-'~                  _..-~;         ;",
              str " `._   _,'     ~`-.._                  ~`-.._           _..-'~     `._    _.- ",
              str "    ~~\"              ~`-.._                  ~`-.._ .-'~              ~~\"~    ",
              str "  .----.            _..-'  ~`-.._                  ~`-.._          .-~~~~-.   ",
              str " /      `.    _..-'~             ~`-.._                  ~`-.._   (        \". ",
              str "Y        `=--~                  _..-'  ~`-.._                  ~`-'         | ",
              str "|                         _..-'~             ~`-.._                         ; ",
              str "`._                 _..-'~                         ~`-.._            -._ _.'  ",
              str "   \"-=\"      _..-'~                                     ~`-.._        ~`.    ",
              str "    /        `.                                                ;          Y   ",
              str "   Y           Y                   --           Y           |   ",
              str "   |           ;                                              `.          /   ",
              str "   `.       _.'                                                 \"-.____.-'    ",
              str "     ~-----\"                                                                 "
            ]
      )

-- Create the map
drawMap :: Game -> Widget Name
drawMap g = vBox [createRow y g | y <- [0 .. gMapRows - 1]]

-- Create the row in map
createRow :: Int -> Game -> Widget Name
createRow y g =
  let mapCells = [setAvailableSize (gRow2Col, 1) $ center $ createCell x y g | x <- [0 .. gMapCols - 1]] -- 生成一行中的每个格子
   in hBox mapCells

-- Create cells in map
createCell :: Int -> Int -> Game -> Widget Name
createCell x y g =
  -- x and y are non-negative, need to be translated to world coordinate
  case renderMountain wx wy g of
    Just mountainWidget -> mountainWidget
    Nothing ->
      case renderMonster wx wy g of
        Just w -> w
        Nothing ->
          if (posX g == wx) && (posY g == wy)
            then str "☺️" -- 用 "☺️" 表示玩家
            else case getEvent wx wy g of
              Nothing -> str " " -- 空白表示空单元格
              Just e -> if shouldDisplayEventIcon e then icon e else str " "
  where
    (cx, cy) = getMapRegionCoord (posX g, posY g)
    wx = x + cx * gMapCols
    wy = y + cy * gMapRows

    shouldDisplayEventIcon :: GameEvent -> Bool
    shouldDisplayEventIcon event = 
      not (isused event) || isFinalConfrontation event

    isFinalConfrontation :: GameEvent -> Bool
    isFinalConfrontation event = 
      name event == "Final Confrontation: The Dark Overlord's Lair"

-- Status Bar
drawStatus :: Game -> Widget n
drawStatus g =
  str ("Press 'q' to quit the game.")
    <=> str ("Position: (" ++ show (posX g) ++ ", " ++ show (posY g) ++ ")")
    <=> str ("Shield: " ++ show (shield g))
    <=> str ("Sword: " ++ show (sword g))
    <=> str ("HP: " ++ show (hp g))
    <=> str ("Attack: " ++ show (attack g))

-- Get current event
getEvent :: Int -> Int -> Game -> Maybe GameEvent
getEvent x y game = find (\e -> eventX e == x && eventY e == y) (getCurrentRegionEvents game)

renderMountain :: Int -> Int -> Game -> Maybe (Widget Name)
renderMountain x y game =
  if isMountainAt x y game
    then Just $ withAttr greenAttr $ str "⛰" -- 用 "⛰" 表示山脉，并应用绿色属性
    else Nothing

isMountainAt :: Int -> Int -> Game -> Bool
isMountainAt x y game = any (\m -> mountainPosX m == x && mountainPosY m == y) (getCurrentRegionMountains game)

-- Event Bar
drawEvent :: Game -> Widget n
drawEvent g =
  case inEvent g of
    Nothing -> str ""
    Just event ->
      if shouldDisplayEvent event
      then
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
      else str ""

shouldDisplayEvent :: GameEvent -> Bool
shouldDisplayEvent event = 
  not (isused event) || isFinalConfrontation event

isFinalConfrontation :: GameEvent -> Bool
isFinalConfrontation event = 
  name event == "Final Confrontation: The Dark Overlord's Lair"


-- debug logs
drawLogs :: [String] -> Widget Name
drawLogs logs = vBox [str s | s <- logs]

drawBattleScreen :: Game -> [Widget Name]
drawBattleScreen game =
  [ vBox
      [ hBox [playerWidget, padLeft (Pad 2) monsterWidget],
        hBorder,
        statusAndEventInfoWidget
      ]
  ]
  where
    playerText =
      unlines
        [ "      _,.",
          "    ,` -.)",
          "   ( _/-\\-._",
          "  /,|`--._,-^|            ,",
          "  \\_| |`-._/||          ,'",
          "    |  `-, / |         /  /",
          "    |     || |        /  /",
          "     `r-._||/   __   /  /",
          " __,-<_     )`-/  `./  /",
          "'  \\   `---'   \\   /  /",
          "    |           |./  /",
          "    /           //  /",
          "\\_/' \\         |/  /",
          " |    |   _,^-'/  /",
          " |    , ``  (\\/  /_",
          "  \\,.->._    \\X-=/^",
          "  (  /   `-._//^`",
          "   `Y-.____(__}",
          "    |     {__}",
          "          ()"
        ]
    playerWidget = strWrap playerText

    monsterText =
      unlines
        [ "·············▄▐·····",
          "·······▄▄▄··▄██▄····",
          "······▐▀█▀▌····▀█▄··",
          "······▐█▄█▌······▀█▄",
          "·······▀▄▀···▄▄▄▄▄▀▀",
          "·····▄▄▄██▀▀▀▀······",
          "····█▀▄▄▄█·▀▀·······",
          "····▌·▄▄▄▐▌▀▀▀······",
          "·▄·▐···▄▄·█·▀▀······",
          "·▀█▌···▄·▀█▀·▀······",
          "········▄▄▐▌▄▄······",
          "········▀███▀█·▄····",
          "·······▐▌▀▄▀▄▀▐▄····",
          "·······▐▀······▐▌···",
          "·······█········█···",
          "······▐▌·········█··"
        ]
    monsterWidget = strWrap monsterText

    statusAndEventInfoWidget = hBox [drawStatus game, padLeft (Pad 2) (drawEvent game)]

ascstring1 :: String
ascstring1 = unlines
    [ "             ,;~;,",
      "                /\\_",
      "               (  /",
      "               (()      //)",
      "               | \\\\  ,,;;'\\",
      "           __ _(  )m=(((((((((((((================--------",
      "         /'  ' '()/~' '.(, |",
      "      ,;(      )||     |  ~",
      "     ,;' \\    /-(.;,   )",
      "          ) /       ) /",
      "         //  PjP    ||",
      "        )_\\         )_\\ -- Liangchun" ]

ascstring2 :: String
ascstring2 = unlines
    [ "                                              ,--,  ,.-.",
      "                ,                   \\,       '-,-`,','-.' | ._",
      "               /|           \\    ,   |\\         }  )/  / `-,',",
      "               [ '          |\\  /|   | |        /  \\|  |/`  ,'`",
      "               | |       ,.`  `,` `, | |  _,...(   (      _',",
      "                \\  \\  __ ,-` `  ,  , `/ |,'      Y     (   \\_L\\",
      "                    \\  \\_\\,``,   ` , ,  /  |         )         _,/",
      "                 \\  '  `  ,_ _`_,-,<._.<        /         /",
      "                  ', `>.,`  `  `   ,., |_      |         /",
      "                    \\/`  `,   `   ,`  | /__,.-`    _,   `\\",
      "                -,-..\\  _  \\  `  /  ,  / `._) _,-\\`       \\",
      "                 \\_,,.) /\\    ` /  / ) (-,, ``    ,        |",
      "                ,` )  | \\_\\       '-`  |  `(               \\",
      "               /  /```(   , --, ,' \\   |`<`    ,            |",
      "              /  /_,--`\\   <\\  V /> ,` )<_/)  | \\      _____)",
      "        ,-, ,`   `   (_,\\ \\    |   /) / __/  /   `----`",
      "       (-, \\           ) \\ ('_.-._)/ /,`    /",
      "       | /  `          `/ \\\\ V   V, /`     /",
      "    ,--\\(        ,     <_/`\\\\     ||      /",
      "   (   ,``-     \\/|         \\-A.A-`|     /",
      "  ,>,_ )_,..(    )\\          -,,_-`  _--`",
      " (_ \\|`   _,/_  /  \\_            ,--`",
      "  \\( `   <.,../`     `-.._   _,-`",
      "   `                      ```-- Fang" ]


ascstring3 :: String
ascstring3 = unlines
    [ "                            ==(W{==========-      /===-                        ",
      "                              ||  (.--.)         /===-_---~~~~~~~~~------____  ",
      "                              | \\_,|**|,__      |===-~___                _,-' `",
      "                 -==\\        `\\ ' `--'   ),    `//~\\   ~~~~`---.___.-~~      ",
      "             ______-==|        /`\\_. .__/\\ \\    | |  \\\\           _-~`         ",
      "       __--~~~  ,-/-==\\      (   | .  |~~~~|   | |   `\\        ,'             ",
      "    _-~       /'    |  \\\\     )__/==0==-\\<>   / /      \\      /               ",
      "  .'        /       |   \\\\      /~\\___/~~\\/  /' /        \\   /'                ",
      " /  ____  /         |    \\`\\.__/-~~   \\  |_/'  /          \\/'                  ",
      "/-'~    ~~~~~---__  |     ~-/~         ( )   /'        _--~`                   ",
      "                  \\_|      /        _) | ;  ),   __--~~                        ",
      "                    '~~--_/      _-~/- |/ \\   '-~ \\                            ",
      "                   {\\__--_/}    / \\_>-|)<__\\      \\                           ",
      "                   /'   (_/  _-~  | |__>--<__|      |                          ",
      "                  |   _/) )-~     | |__>--<__|      |                          ",
      "                  / /~ ,_/       / /__>---<__/      |                          ",
      "                 o-o _//        /-~_>---<__-~      /                           ",
      "                (^(~          /~_>---<__-      _-~                            ",
      "               ,/|           /__>--<__/     _-~                               ",
      "             ,//('(          |__>--<__|     /                   .----_          ",
      "            ( ( '))          |__>--<__|    |                 /' _---_~\\        ",
      "         `-)) )) (           |__>--<__|    |               /'  /     ~\\`\\      ",
      "        ,/,'//( (             \\__>--<__\\    \\            /'  //        ||      ",
      "      ,( ( ((, ))              ~-__>--<_~-_  ~--____---~' _/'/        /'       ",
      "    `~/  )` ) ,/|                 ~-_~>--<_/-__       __-~ _/                  ",
      "  ._-~//( )/ )) `                    ~~-'_/_/ /~~~~~~~__--~                    ",
      "   ;'( ')/ ,)(                              ~~~~~~~~~~                         ",
      "  ' ') '( (/                                                                    ",
      "    '   '  `                                                                    -- dongming"
    ]


ascstring4 :: String
ascstring4 = unlines
    [ "                            _.--.",
      "                        _.-'_:-'||",
      "                    _.-'_.-::::'||",
      "               _.-:'_.-::::::'  ||",
      "             .'`-.-:::::::'     ||",
      "            /.'`;|:::::::'      ||_",
      "           ||   ||::::::'     _.;._'-._",
      "           ||   ||:::::'  _.-!oo @.!-._'-.",
      "           \'.  ||:::::.-!()oo @!()@.-'_.|",
      "            '.'-;|:.-'.&$@.& ()$%-'o.' U||",
      "              `>'-.!@%()@'@_%-'_.-o _.|'||",
      "               ||-._'-.@.-'_.-' _.-o  |'||",
      "               ||=[ '-._.-\\U/.-'    o |'||",
      "               || '-.]=|| |'|      o  |'||",
      "               ||      || |'|        _| ';",
      "               ||      || |'|    _.-'_.-'",
      "               |'-._   || |'|_.-'_.-'",
      "      --zhiqiao '-._'-.|| |' `_.-'",
      "                    '-.||_/.-'"
    ]


