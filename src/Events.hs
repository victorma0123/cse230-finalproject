{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}

module Events where

import Brick (str)
import Types

abandonedCampsite :: GameEvent
abandonedCampsite =
  GEvent
    { eventX = 5,
      eventY = 5,
      name = "Abandoned Campsite",
      description = "You come across an abandoned campsite.",
      choices = [searchForSuppliesChoice, leaveUndisturbedChoice],
      icon = str "🏘"
    }

searchForSuppliesChoice :: EventChoice
searchForSuppliesChoice =
  GChoice
    { title = "Search for Supplies",
      effect = \game -> game {hp = hp game + 5, sword = sword game + 1}
    }

leaveUndisturbedChoice :: EventChoice
leaveUndisturbedChoice =
  GChoice
    { title = "Leave Undisturbed",
      effect = id -- No effect
    }

hermitsHut :: GameEvent
hermitsHut =
  GEvent
    { eventX = 14,
      eventY = 20,
      name = "Hermit's Hut",
      description = "A wise hermit lives in a secluded hut.",
      choices = [seekAdviceChoice, leaveHermitChoice],
      icon = str "⌂"
    }

seekAdviceChoice :: EventChoice
seekAdviceChoice =
  GChoice
    { title = "Seek Advice",
      effect = \game -> game {attack = attack game + 3}
    }

leaveHermitChoice :: EventChoice
leaveHermitChoice =
  GChoice
    { title = "Leave Hermit in Peace",
      effect = id -- No effect
    }

potionVendorCart :: GameEvent
potionVendorCart =
  GEvent
    { eventX = 8,
      eventY = 22,
      name = "Potion Vendor's Cart",
      description = "A colorful cart belonging to a potion vendor appears on the road.",
      choices = [buyPotionChoice, haggleChoice],
      icon = str "𐋃"
      -- icon = str "🛒"
    }

buyPotionChoice :: EventChoice
buyPotionChoice =
  GChoice
    { title = "Buy a Potion",
      effect = \game -> game {hp = hp game + 15}
    }

haggleChoice :: EventChoice
haggleChoice =
  GChoice
    { title = "Haggle for a Better Deal",
      effect = \game ->
        if even (posX game + posY game)
          then game {hp = hp game - 5} -- Unsuccessful haggling
          else game {hp = hp game + 10} -- Successful haggling
    }

bardsPerformance :: GameEvent
bardsPerformance =
  GEvent
    { eventX = 16,
      eventY = 12,
      name = "Bard's Performance",
      description = "A talented bard is performing in a bustling town square.",
      choices = [enjoyPerformanceChoice, tipBardChoice],
      icon = str "🎜"
      -- icon = str "🎵"
    }

enjoyPerformanceChoice :: EventChoice
enjoyPerformanceChoice =
  GChoice
    { title = "Enjoy the Performance",
      effect = \game -> game {attack = attack game + 2}
    }

tipBardChoice :: EventChoice
tipBardChoice =
  GChoice
    { title = "Tip the Bard",
      effect = \game -> game {hp = hp game + 5, attack = attack game + 1}
    }

fortuneTellersTent :: GameEvent
fortuneTellersTent =
  GEvent
    { eventX = 12,
      eventY = 4,
      name = "Fortune Teller's Tent",
      description = "A mysterious fortune teller invites you into her tent.",
      choices = [fortuneToldChoice, declineChoice],
      -- icon = str "🔮"
      icon = str "൜"
    }

fortuneToldChoice :: EventChoice
fortuneToldChoice =
  GChoice
    { title = "Have Your Fortune Told",
      effect = \game -> game {hp = hp game + 10}
    }

declineChoice :: EventChoice
declineChoice =
  GChoice
    { title = "Decline and Leave",
      effect = id -- No effect
    }

smithyEncounter :: GameEvent
smithyEncounter =
  GEvent
    { eventX = 5,
      eventY = 12,
      name = "Smithy Encounter",
      description = "A skilled blacksmith offers to enhance your weapons and armor.",
      choices = [enhanceWeaponChoice, reinforceArmorChoice],
      icon = str "☭"
      -- icon = str "🔨"
    }

enhanceWeaponChoice :: EventChoice
enhanceWeaponChoice =
  GChoice
    { title = "Enhance Weapon",
      effect = \game -> game {attack = attack game + 5}
    }

reinforceArmorChoice :: EventChoice
reinforceArmorChoice =
  GChoice
    { title = "Reinforce Armor",
      effect = \game -> game {shield = shield game + 7}
    }

elderlyExplorerAdvice :: GameEvent
elderlyExplorerAdvice =
  GEvent
    { eventX = 9,
      eventY = 14,
      name = "Elderly Explorer's Advice",
      description = "An elderly explorer shares tales of ancient artifacts and hidden treasures.",
      choices = [listenAndLearnChoice, dismissAndContinueChoice],
      icon = str "🗺️"
    }

listenAndLearnChoice :: EventChoice
listenAndLearnChoice =
  GChoice
    { title = "Listen and Learn",
      effect = \game -> game {sword = sword game + 5}
    }

dismissAndContinueChoice :: EventChoice
dismissAndContinueChoice =
  GChoice
    { title = "Dismiss and Continue",
      effect = id -- No effect
    }

cursedRelicEncounter :: GameEvent
cursedRelicEncounter =
  GEvent
    { eventX = 5,
      eventY = 20,
      name = "Cursed Relic Encounter",
      description = "You stumble upon an ancient relic emitting an ominous aura.",
      choices = [takeRelicChoice, leaveRelicChoice],
      icon = str "🜘"
      -- icon = str "🔱"
    }

takeRelicChoice :: EventChoice
takeRelicChoice =
  GChoice
    { title = "Take the Relic",
      effect = \game -> game {hp = hp game - 10, attack = attack game + 5}
    }

leaveRelicChoice :: EventChoice
leaveRelicChoice =
  GChoice
    { title = "Leave the Relic Untouched",
      effect = id -- No effect
    }

mysteriousFountain :: GameEvent
mysteriousFountain =
  GEvent
    { eventX = 13,
      eventY = 22,
      name = "Mysterious Fountain",
      description = "A fountain shrouded in mystery stands in the center of a clearing.",
      choices = [drinkFromFountainChoice, walkAwayChoice],
      icon = str "🜄"
      -- icon = str "⛲"
    }

drinkFromFountainChoice :: EventChoice
drinkFromFountainChoice =
  GChoice
    { title = "Drink from the Fountain",
      effect = \game -> game {hp = hp game + 20, shield = shield game + 10}
    }

walkAwayChoice :: EventChoice
walkAwayChoice =
  GChoice
    { title = "Walk Away",
      effect = id -- No effect
    }

allEvents :: [GameEvent]
allEvents =
  [ abandonedCampsite,
    hermitsHut,
    potionVendorCart,
    bardsPerformance,
    fortuneTellersTent,
    smithyEncounter,
    elderlyExplorerAdvice,
    cursedRelicEncounter,
    mysteriousFountain
  ]

allMonsters :: [Monster]
allMonsters =
  [ Monster 5 5 "Goblin Raider" 30 8,
    Monster 5 10 "Forest Nymph" 20 6,
    Monster 5 15 "Mountain Troll" 50 10,
    Monster 10 5 "Shadow Assassin" 40 12
  ]