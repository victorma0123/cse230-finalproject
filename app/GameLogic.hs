module GameLogic where

import Brick (str)
import Debug (appendKeyValueLog, appendLogsToGame)
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
    { eventX = 10,
      eventY = 10,
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
      icon = str "⏾"
    }

-- Monster Encounter
goblinRaiderEvent :: GameEvent
goblinRaiderEvent =
  GEvent
    { eventX = -1,
      eventY = -1,
      name = "Goblin Raider",
      description = "A sneaky Goblin Raider jumps out!",
      choices = [fightChoice, useItemChoice, fleeChoice],
      icon = str "G"
    }

forestNymphEvent :: GameEvent
forestNymphEvent =
  GEvent
    { eventX = -1,
      eventY = -1,
      name = "Forest Nymph",
      description = "A mystical Forest Nymph appears!",
      choices = [fightChoice, useItemChoice, fleeChoice],
      icon = str "F"
    }

mountainTrollEvent :: GameEvent
mountainTrollEvent =
  GEvent
    { eventX = -1,
      eventY = -1,
      name = "Mountain Troll",
      description = "A formidable Mountain Troll blocks your path!",
      choices = [fightChoice, useItemChoice, fleeChoice],
      icon = str "T"
    }

shadowAssassinEvent :: GameEvent
shadowAssassinEvent =
  GEvent
    { eventX = -1,
      eventY = -1,
      name = "Shadow Assassin",
      description = "A deadly Shadow Assassin emerges from the shadows!",
      choices = [fightChoice, useItemChoice, fleeChoice],
      icon = str "S"
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

updateMonstersInGame :: [Monster] -> Game -> Game
updateMonstersInGame m game = game {monsters = m}

fightMonster :: Game -> Game
fightMonster game =
  updateGameState $
    case inMonster game of
      Just monster ->
        let currentMonster = monster
            newPlayerHp = max 0 (hp game - monsterAttack monster)
            newMonsterHp = max 0 (monsterHp monster - attack game)
            updatedMonster = monster {monsterHp = newMonsterHp}
            updatedMonsters = replaceMonsterInList monster updatedMonster (monsters game)
            gameUpdatedMonster = game {monsters = updatedMonsters}
            isMonsterDefeated = newMonsterHp <= 0
            finalMonsters = if isMonsterDefeated then filter (not . gameMonsterEqual monster) updatedMonsters else monsters game
            gameOverUpdate = hp game == 0
            bonusGain = getBonusForMonster (monsterName monster)
            evt = if isMonsterDefeated then Nothing else inEvent gameUpdatedMonster
            updatedGame = if isMonsterDefeated then applyBonus bonusGain gameUpdatedMonster else gameUpdatedMonster
         in updatedGame
              { hp = newPlayerHp,
                monsters = finalMonsters,
                gameOver = gameOverUpdate,
                inEvent = evt,
                inMonster = if isMonsterDefeated then Nothing else Just updatedMonster,
                inBattle = not isMonsterDefeated -- 设置 inBattle 根据怪物是否被击败
              }
      Nothing -> game


applyBonus :: Bonus -> Game -> Game
applyBonus bonus game =
  -- define how bonuses are applied
  case bonus of
    NoBonus -> game
    HPBonus bonusAmount -> game {hp = min 100 (hp game + bonusAmount)}
    AttackBonus bonusAmount -> game {attack = attack game + bonusAmount}
    ShieldBonus bonusAmount -> game {shield = min 100 (shield game + bonusAmount)}
    SwordBonus bonusAmount -> game {sword = sword game + bonusAmount}

getBonusForMonster :: String -> Bonus
getBonusForMonster monsterName =
  case monsterName of
    "Goblin Raider" -> ShieldBonus 5
    "Forest Nymph" -> SwordBonus 3
    "Mountain Troll" -> HPBonus 10
    "Shadow Assassin" -> AttackBonus 4
    _ -> NoBonus

replaceMonsterInList :: Monster -> Monster -> [Monster] -> [Monster]
replaceMonsterInList oldMonster newMonster = map (\m -> if gameMonsterEqual m oldMonster then newMonster else m)

useItem :: Game -> Game
useItem game =
  let healthPotionEffect = 20
      newHp = min 100 (hp game + healthPotionEffect)
   in game {hp = newHp}

flee :: Game -> Game
flee game = game {hp = max 0 (hp game - 5), inEvent = Nothing, inBattle = False}

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
    { eventX = 10,
      eventY = 15,
      name = "Treasure Chest",
      description = "You've found a treasure chest!",
      choices = [openChestChoice],
      icon = str "⛝"
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
        then game {shield = min 100 (shield game + shieldBonus)} -- Position x divisible by 3 gives shield
        else game {hp = max 0 (hp game - trapDamage)} -- Other positions are traps
  where
    healthBonus = 20
    shieldBonus = 15
    trapDamage = 10

-- Ancient Shrine
ancientShrineEncounter :: GameEvent
ancientShrineEncounter = GEvent
  { eventX = 7,
    eventY = 7,
    name = "Ancient Shrine Encounter",
    description = "You encounter a mysterious ancient shrine in the forest.",
    choices = [offerStrengthChoice, meditateChoice],
    icon = str "۩"
  }

offerStrengthChoice :: EventChoice
offerStrengthChoice = GChoice
  { title = "Offer Strength",
    effect = \game -> 
      if attack game > 3 
      then game { attack = attack game - 3, sword = sword game + 10 } 
      else game
  }

meditateChoice :: EventChoice
meditateChoice = GChoice
  { title = "Meditate",
    effect = \game -> game { shield = shield game + 5 }
  }

-- Mysterious Traveler
mysteriousTraveler :: GameEvent
mysteriousTraveler = GEvent
  { eventX = 14,
    eventY = 7,
    name = "Mysterious Traveler",
    description = "You meet a mysterious traveler at a crossroads.",
    choices = [shareMealChoice, trainTogetherChoice],
    icon = str "⚇"
  }

shareMealChoice :: EventChoice
shareMealChoice = GChoice
  { title = "Share a meal",
    effect = \game -> game { hp = hp game - 10, shield = shield game + 10 }
  }

trainTogetherChoice :: EventChoice
trainTogetherChoice = GChoice
  { title = "Train together",
    effect = \game -> game { attack = attack game + 3 }
  }

-- Lost Treasure Chest
lostTreasureChest :: GameEvent
lostTreasureChest = GEvent
  { eventX = 16,
    eventY = 4,
    name = "Lost Treasure Chest",
    description = "You find a lost treasure chest in a hidden cave.",
    choices = [forceOpenChoice, carefullyUnlockChoice],
    icon = str "⛝"
  }

forceOpenChoice :: EventChoice
forceOpenChoice = GChoice
  { title = "Force open",
    effect = \game -> 
      if attack game > 20 
      then game { hp = hp game + 5, shield = shield game + 5 } 
      else game
  }

carefullyUnlockChoice :: EventChoice
carefullyUnlockChoice = GChoice
  { title = "Carefully unlock",
    effect = \game -> 
      if sword game > 15 
      then game { hp = hp game + 10 }  -- Assuming finding gold impacts hp positively
      else game
  }


--  Enchanted Lake
enchantedLake :: GameEvent
enchantedLake = GEvent
  { eventX = 3,
    eventY = 13,
    name = "Enchanted Lake",
    description = "You discover an enchanted lake that glows under the moonlight.",
    choices = [batheInLakeChoice, searchAroundChoice],
    icon = str "〰"
  }

batheInLakeChoice :: EventChoice
batheInLakeChoice = GChoice
  { title = "Bathe in the lake",
    effect = \game -> game { hp = 150 }  -- Assuming full heal plus extra HP
  }

searchAroundChoice :: EventChoice
searchAroundChoice = GChoice
  { title = "Search around the lake",
    effect = \game -> game -- Implementation depends on what the random bonuses are
  }

-- Ancient Library
ancientLibrary :: GameEvent
ancientLibrary = GEvent
  { eventX = 2,
    eventY = 9,
    name = "The Ancient Library",
    description = "You find yourself in a library filled with ancient tomes.",
    choices = [studyAncientTomesChoice, searchForSecretsChoice],
    icon = str "𐂨"
  }

studyAncientTomesChoice :: EventChoice
studyAncientTomesChoice = GChoice
  { title = "Study ancient tomes",
    effect = \game -> 
      if sword game >= 10  -- Adjust the threshold for 'high Sword' as needed
      then game { attack = attack game + 5 } 
      else game
  }

searchForSecretsChoice :: EventChoice
searchForSecretsChoice = GChoice
  { title = "Search for secret passages",
    effect = \game -> game -- Effect depends on how you want to handle map discovery
  }

-- Final Confrontation: The Dark Overlord's Lair Event
finalConfrontation :: GameEvent
finalConfrontation = GEvent
  { eventX = 18, 
    eventY = 13,
    name = "Final Confrontation: The Dark Overlord's Lair",
    description = "You stand before the lair of the Dark Overlord, ready for the final battle.",
    choices = [directAssaultChoice],
    icon = str "D"
  }

directAssaultChoice :: EventChoice
directAssaultChoice = GChoice
  { title = "Direct assault",
    effect = \game ->
      if hp game >= 150 && shield game >= 20 && attack game >= 20 && sword game >= 15
      then game {winner = True}
      else game {loser = True}
  }
