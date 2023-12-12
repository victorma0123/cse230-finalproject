module GameLogic where

import Brick (str)
import System.Random (randomRIO)
import Types

-- current implementation only checks whether two monsters are at the
-- same position. An corner case is that two (different) monster may
-- randomly walk to the same position. And in that case, when calling
-- gameMonsterEqual to remove the defeated monter, two monsters will
-- all be removed.
-- TODO: fix the corner case

-- Sleep Event
sleepEvent :: GameEvent
sleepEvent =
  GEvent
      { eventX = 5,
        eventY = 5,
        name = "sleep!",
        description = "Sleeping will help recover HP",
        choices =
          [ GChoice
              { title = "sleep for 10 hours",
                effect = \g -> g {hp = hp g + 2}
              },
            GChoice
              { title = "sleep for 5 hours",
                effect = \g -> g {hp = hp g + 1}
              }
          ],
        icon = str "s"
      }

-- Monster Encounter
monsterEncounterEvent :: GameEvent
monsterEncounterEvent =
  GEvent
    { eventX = -1,
      eventY = -1,
      name = "Monster Encounter",
      description = "A wild creature appears!",
      choices = [fightChoice, useItemChoice, fleeChoice],
      icon = str "M"
    }

gameMonsterEqual :: Monster -> Monster -> Bool
gameMonsterEqual m1 m2 =
  monsterPosX m1 == monsterPosX m2
    && monsterPosY m1 == monsterPosY m2

fightChoice :: EventChoice
fightChoice = GChoice {title = "Fight", effect = fightMonster}

useItemChoice :: EventChoice
useItemChoice = GChoice {title = "Use Item", effect = useItem}

fleeChoice :: EventChoice
fleeChoice = GChoice {title = "Flee", effect = flee}

updateGameState :: Game -> Game
updateGameState game =
  if hp game <= 0
    then game {gameOver = True}
    else game

fightMonster :: Game -> Game
fightMonster game =
  updateGameState $
    case inMonster game of
      Just monster ->
        let monsterHp = 30
            monsterAttack = 20
            newPlayerHp = max 0 (hp game - monsterAttack)
            isMonsterDefeated = attack game >= monsterHp
            gameOverUpdate = hp game == 0
            -- remove the defeated monster (not the monster event!)
            -- Ideally, we can add a type in event to indicate this is an monster (that can move!).
            -- Then we can remove the corresponding event for the monster
            -- But in current code, monsters are represented as another type (Monster), which all share
            -- the same monsterEncounterEvent (which will not be rendered in the map).
            -- To remove a monster in map, we need to remove it from the monsters, not events.
            remainMonsters = if isMonsterDefeated then filter (not . gameMonsterEqual monster) (monsters game) else monsters game
            -- also set inEvent to Nothing, otherwise, even if the monster is defeated, the UI (event bar) will not be updated
            -- TODO: fix the corner case, where multiple monsters are at the same position. In that case, we cannot set inEvent to Nothing
            evt = if isMonsterDefeated then Nothing else inEvent game
         in game {hp = newPlayerHp, monsters = remainMonsters, gameOver = gameOverUpdate, inEvent = evt}
      Nothing -> game

useItem :: Game -> Game
useItem game =
  let healthPotionEffect = 20
      newHp = min 100 (hp game + healthPotionEffect)
   in game {hp = newHp}

flee :: Game -> Game
flee game = game {hp = max 0 (hp game - 5), inEvent = Nothing}

moveMonster :: Monster -> Game -> IO Monster
moveMonster monster game = do
  direction <- randomRIO (1, 4) :: IO Int
  let (dx, dy) = case direction of
        1 -> (0, 1) -- Move up
        2 -> (0, -1) -- Move down
        3 -> (-1, 0) -- Move left
        4 -> (1, 0) -- Move right
      newX = monsterPosX monster + dx
      newY = monsterPosY monster + dy
      isMountain = any (\m -> mountainPosX m == newX && mountainPosY m == newY) (mountains game)
  if isMountain
    then return monster -- 如果新位置有山脉，怪物保持不动
    else return $ monster {monsterPosX = newX, monsterPosY = newY}


-- Treasure Chest
treasureChest :: GameEvent
treasureChest =
  GEvent
    { eventX = 15,
      eventY = 15,
      name = "Treasure Chest",
      description = "You've found a treasure chest!",
      choices = [openChestChoice],
      icon = str "✩"
    }

openChestChoice :: EventChoice
openChestChoice =
  GChoice
    { title = "Open Chest",
      effect = openChest
    }

openChest :: Game -> Game
openChest game =
  if even (posX game + posY game) -- Using player's position to determine the outcome
    then game {hp = min 150 (hp game + healthBonus)} -- Even position sums give health
    else
      if posX game `mod` 3 == 0
        then game {sheild = min 100 (sheild game + shieldBonus)} -- Position x divisible by 3 gives shield
        else game {hp = max 0 (hp game - trapDamage)} -- Other positions are traps
  where
    healthBonus = 20
    shieldBonus = 15
    trapDamage = 10


