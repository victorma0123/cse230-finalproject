module Spec where

{-# LANGUAGE TemplateHaskell #-}
import Test.HUnit
import GameLogic
import Types

-- a simple test (unfinished)

-- Custom comparison for GameEvent
compareGameEvents :: GameEvent -> GameEvent -> Bool
compareGameEvents event1 event2 =
    eventX event1 == eventX event2 &&
    eventY event1 == eventY event2 &&
    name event1 == name event2 &&
    description event1 == description event2
    -- Add comparisons for other fields, but skip non-comparable fields like 'icon'

-- Custom comparison for Game
compareGames :: Game -> Game -> Bool
compareGames game1 game2 =
    posX game1 == posX game2 &&
    posY game1 == posY game2
    -- Add comparisons for other fields


-- Helper function to create a GameEvent without the icon for testing
createTestGameEvent eventX eventY name description choices = GEvent
    { eventX = eventX,
      eventY = eventY,
      name = name,
      description = description,
      choices = choices,
      icon = undefined -- Mocking the icon field as it cannot be compared directly
    }

-- Test for the 'goblinRaiderEvent' function
testGoblinRaiderEvent = TestCase $ do
    let expected = createTestGameEvent (-1) (-1) "Goblin Raider" "A sneaky Goblin Raider jumps out!" [fightChoice, useItemChoice, fleeChoice]
    let actual = goblinRaiderEvent { icon = undefined } -- Mocking the icon field in the actual value too
    assertBool "Testing goblinRaiderEvent" (compareGameEvents expected actual)

-- Test for the 'treasureChest' function
testTreasureChest = TestCase $ do
    let expected = createTestGameEvent 10 15 "Treasure Chest" "You've found a treasure chest!" [openChestChoice]
    let actual = treasureChest { icon = undefined } -- Mocking the icon field in the actual value too
    assertBool "Testing treasureChest" (compareGameEvents expected actual)



tests = TestList [TestLabel "testGoblinRaiderEvent" testGoblinRaiderEvent,
                  TestLabel "testTreasureChest" testTreasureChest]

main = runTestTT tests
