module GameLogic where

import Types
import System.Random (randomRIO)
import Brick (str)

gameEventsEqual :: GameEvent -> GameEvent -> Bool
gameEventsEqual e1 e2 = 
  eventX e1 == eventX e2 &&
  eventY e1 == eventY e2 &&
  name e1 == name e2

monsterEncounterEvent :: GameEvent
monsterEncounterEvent = GEvent
  { eventX = -1,
    eventY = -1,
    name = "Monster Encounter",
    description = "A wild creature appears!",
    choices = [fightChoice, useItemChoice, fleeChoice],
    icon = str "M" 
  }

fightChoice = GChoice { title = "Fight", effect = fightMonster }
useItemChoice = GChoice { title = "Use Item", effect = useItem }
fleeChoice = GChoice { title = "Flee", effect = flee }

fightMonster :: Game -> Game
fightMonster game =
  case inEvent game of
    Just currentEvent ->
      let monsterHp = 30
          monsterAttack = 20
          newPlayerHp = max 0 (hp game - monsterAttack)
          isMonsterDefeated = attack game >= monsterHp
          updatedEvents = if isMonsterDefeated then filter (gameEventsEqual currentEvent) (events game) else events game
      in game { hp = newPlayerHp, events = updatedEvents }
    Nothing -> game


useItem :: Game -> Game
useItem game =
  let healthPotionEffect = 20
      newHp = min 100 (hp game + healthPotionEffect)
  in game { hp = newHp }

flee :: Game -> Game
flee game = game { hp = max 0 (hp game - 5) }

treasureChest :: GameEvent
treasureChest = GEvent
  { eventX = 15,
    eventY = 15,
    name = "Treasure Chest",
    description = "You've found a treasure chest!",
    choices = [openChestChoice],
    icon = str "T"
  }

openChestChoice :: EventChoice
openChestChoice = GChoice 
  { title = "Open Chest",
    effect = openChest
  }

openChest :: Game -> Game
openChest game =
  if even (posX game + posY game)  -- Using player's position to determine the outcome
    then game { hp = min 100 (hp game + healthBonus) } -- Even position sums give health
    else if posX game `mod` 3 == 0
         then game { sheild = min 100 (sheild game + shieldBonus) } -- Position x divisible by 3 gives shield
         else game { hp = max 0 (hp game - trapDamage) } -- Other positions are traps
  where
    healthBonus = 20
    shieldBonus = 15
    trapDamage = 10

moveMonster :: Monster -> Int -> Int -> IO Monster
moveMonster monster mapWidth mapHeight = do
  direction <- randomRIO (1, 4) :: IO Int
  let (dx, dy) = case direction of
        1 -> (0, 1)  -- Move up
        2 -> (0, -1) -- Move down
        3 -> (-1, 0) -- Move left
        4 -> (1, 0)  -- Move right
      newX = max 0 $ min (mapWidth - 1) $ monsterPosX monster + dx
      newY = max 0 $ min (mapHeight - 1) $ monsterPosY monster + dy
  return $ monster { monsterPosX = newX, monsterPosY = newY }