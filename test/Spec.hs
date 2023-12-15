import Test.HUnit
import Types
import GameLogic (updateGameState, goblinRaiderEvent, forestNymphEvent, mountainTrollEvent, shadowAssassinEvent, treasureChest, ancientShrineEncounter, mysteriousTraveler, lostTreasureChest, enchantedLake, ancientLibrary, finalConfrontation, fightMonster, applyBonus, moveMonster, updateGameState, sleepEvent, meditateChoice)
import Init (initialState, initialEvents)
import Brick.Widgets.Core (emptyWidget)
import Data.Maybe (isJust, isNothing)
import Data.List (find)
import Brick (str)


-- Mock functions and data
mockMonster :: Monster
mockMonster = Monster 5 5 "Mock Monster" 30 10

mockEvent :: GameEvent
mockEvent = GEvent 5 5 "Mock Event" "Just a mock event" [] emptyWidget False

-- Initialize a mock game state for testing purposes
initialMockState :: Game
initialMockState = Game
  { posX = 0, posY = 0, shield = 10, sword = 10, hp = 100, attack = 15,
    events = [mockEvent], iChoice = 0, inEvent = Just mockEvent,
    monsters = [mockMonster], inMonster = Just mockMonster,
    gameOver = False, winner = False, loser = False,
    mountains = [], displayLogs = False, inBattle = False, logs = [] }

-- Test Initialization
testPosInitialization :: Test
testPosInitialization = TestCase $ do
    let initState = initialMockState
    assertEqual "Initial position X should be 0" 0 (posX initState)
    assertEqual "Initial position Y should be 0" 0 (posY initState)

testHPInitialization :: Test
testHPInitialization = TestCase $ do
    let initState = initialMockState
    assertEqual "Initial HP should be 100" 100 (hp initState)

testShieldInitialization :: Test
testShieldInitialization = TestCase $ do
    let initState = initialMockState
    assertEqual "Initial Shield should be 10" 10 (shield initState)

testSwordInitialization :: Test
testSwordInitialization = TestCase $ do
    let initState = initialMockState
    assertEqual "Initial Sword should be 10" 10 (sword initState)

testAttackInitialization :: Test
testAttackInitialization = TestCase $ do
    let initState = initialMockState
    assertEqual "Initial Attack should be 15" 15 (attack initState)

testIChoiceInitialization :: Test
testIChoiceInitialization = TestCase $ do
    let initState = initialMockState
    assertEqual "Initial IChoice should be 0" 0 (iChoice initState)

testGameOverInitialization :: Test
testGameOverInitialization = TestCase $ do
    let initState = initialMockState
    assertBool "Game should not be over at initialization" (not $ gameOver initState)

testWinnerInitialization :: Test
testWinnerInitialization = TestCase $ do
    let initState = initialMockState
    assertBool "Game should not be won at initialization" (not $ winner initState)

testLoserInitialization :: Test
testLoserInitialization = TestCase $ do
    let initState = initialMockState
    assertBool "Game should not be lost at initialization" (not $ loser initState)

testMountainsInitialization :: Test
testMountainsInitialization = TestCase $ do
    let initState = initialMockState
    assertEqual "Initial Mountains should be empty" [] (mountains initState)

testDisplayLogsInitialization :: Test
testDisplayLogsInitialization = TestCase $ do
    let initState = initialMockState
    assertBool "Display Logs should be False at initialization" (not $ displayLogs initState)

testInBattleInitialization :: Test
testInBattleInitialization = TestCase $ do
    let initState = initialMockState
    assertBool "In Battle should be False at initialization" (not $ inBattle initState)

testLogsInitialization :: Test
testLogsInitialization = TestCase $ do
    let initState = initialMockState
    assertEqual "Initial Logs should be empty" [] (logs initState)

-- Test HP and Shield Limits
testHpLimit :: Test
testHpLimit = TestCase $ do
    let initialStateWithHighHp = initialState { hp = 95 } -- Setting HP close to the limit
    let sleepChoice = head (choices sleepEvent) -- Get the first choice from the sleep event
    let updatedGame = applyChoiceEffect initialStateWithHighHp sleepChoice -- Applying the choice
    assertBool "HP should not exceed 100" (hp updatedGame <= 100)

-- Mock event to test shield limit
mockShieldIncreaseEvent :: GameEvent
mockShieldIncreaseEvent = GEvent
    { eventX = 0,
      eventY = 0,
      name = "Mock Shield Increase",
      description = "Increases player's shield",
      choices = [mockShieldIncreaseChoice],
      icon = str "🛡",
      isused = False
    }

mockShieldIncreaseChoice :: EventChoice
mockShieldIncreaseChoice = GChoice
    { title = "Increase Shield",
      effect = \game -> game { shield = min 100 (shield game + 10) } -- Increase shield by 10, up to a max of 100
    }

testShieldLimit :: Test
testShieldLimit = TestCase $ do
    let initialStateWithHighShield = initialState { shield = 95 } -- Setting Shield close to the limit
    let mockChoice = head (choices mockShieldIncreaseEvent) -- Get the first choice from the mock event
    let updatedGame = applyChoiceEffect initialStateWithHighShield mockChoice
    assertBool "Shield should not exceed 100" (shield updatedGame <= 100)

-- Test Monster Behavior
testMonsterBehavior :: Test
testMonsterBehavior = TestCase $ do
    let initState = initialMockState
    let monsterState = fightMonster initState
    assertBool "Monster should be engaged after fight" (isJust $ inMonster monsterState)

-- Test Boundary and Overlap Checks
testBoundaries :: Test
testBoundaries = TestCase $ do
    let initState = initialMockState
    let outOfBounds = posX initState < 0 || posY initState < 0 || posX initState >= 20 || posY initState >= 20
    assertBool "Player should be within game boundaries" (not outOfBounds)

-- Helper function to simulate encountering an event
simulateEventEncounter :: Game -> GameEvent -> Game
simulateEventEncounter game event =
    game { posX = eventX event, posY = eventY event, inEvent = Just event }

-- Function to create a test for a given event
createEventTest :: String -> GameEvent -> Test
createEventTest eventName event = TestCase $ do
    let gameWithEvent = simulateEventEncounter initialState event
    let encounterState = updateGameState gameWithEvent
    assertBool (eventName ++ " should be encountered") (isJust $ inEvent encounterState)
    -- Add more assertions here as needed based on the event's effects

-- Creating tests for all events
testGoblinRaiderEncounter = createEventTest "Goblin Raider" goblinRaiderEvent
testForestNymphEncounter = createEventTest "Forest Nymph" forestNymphEvent
testMountainTrollEncounter = createEventTest "Mountain Troll" mountainTrollEvent
testShadowAssassinEncounter = createEventTest "Shadow Assassin" shadowAssassinEvent
testTreasureChestEncounter = createEventTest "Treasure Chest" treasureChest
testAncientShrineEncounter = createEventTest "Ancient Shrine" ancientShrineEncounter
testMysteriousTravelerEncounter = createEventTest "Mysterious Traveler" mysteriousTraveler
testLostTreasureChestEncounter = createEventTest "Lost Treasure Chest" lostTreasureChest
testEnchantedLakeEncounter = createEventTest "Enchanted Lake" enchantedLake
testAncientLibraryEncounter = createEventTest "Ancient Library" ancientLibrary
testFinalConfrontationEncounter = createEventTest "Final Confrontation" finalConfrontation

-- Apply a choice effect to a game state
applyChoiceEffect :: Game -> EventChoice -> Game
applyChoiceEffect game choice = effect choice game

-- Test a specific choice from an event
testEventChoice :: String -> GameEvent -> Int -> Game -> (Game -> Bool) -> Test
testEventChoice testName event choiceIndex initialState condition = TestCase $ do
    let choice = choices event !! choiceIndex
    let updatedGame = applyChoiceEffect initialState choice
    assertBool testName (condition updatedGame)

-- Adjusted condition functions
isHpIncreasedByTwo :: Game -> Bool
isHpIncreasedByTwo game = hp game == hp initialStateWithLowerHp + 2

-- Check if the player's HP is increased
isHpIncreased :: Game -> Bool
isHpIncreased game = hp game > hp initialStateWithLowerHp || hp game == 100

-- Make sure initialStateWithLowerHp has less than maximum HP to see the increase
initialStateWithLowerHp = initialState { hp = 50 }

-- Assuming initialState has sufficient HP and a monster present
initialStateWithMonster = initialState { inMonster = Just (head (monsters initialState)), hp = 100 }

-- Check if the player's HP is reduced after fighting a monster
isHpReduced :: Game -> Bool
isHpReduced game = hp game < 100 -- Assuming initial HP is 100

-- Check if the game is over
isGameOver :: Game -> Bool
isGameOver game = gameOver game

-- Assuming initial states for different scenarios
initialStateWithHealth = initialState { hp = 100 }
initialStateWithAttack = initialState { attack = 15 }

-- Condition to check if the shield is increased
isShieldIncreased :: Game -> Bool
isShieldIncreased game = shield game > shield initialState

-- Condition to check if the attack is increased
isAttackIncreased :: Game -> Bool
isAttackIncreased game = attack game > attack initialState

-- Condition functions
isAttackReducedAndSwordIncreased :: Game -> Bool
isAttackReducedAndSwordIncreased game = attack game < attack initialState && sword game > sword initialState

-- Condition to check if the player exited an event
hasExitedEvent :: Game -> Bool
hasExitedEvent game = isNothing (inEvent game)

-- Tests for Forest Nymph Event
testForestNymphFightChoice = testEventChoice "Forest Nymph Fight" forestNymphEvent 0 initialStateWithMonster isHpReduced
testForestNymphUseItemChoice = testEventChoice "Forest Nymph Use Item" forestNymphEvent 1 initialStateWithHealth isHpIncreased
testForestNymphFleeChoice = testEventChoice "Forest Nymph Flee" forestNymphEvent 2 initialState hasExitedEvent

-- Tests for Mountain Troll Event
testMountainTrollFightChoice = testEventChoice "Mountain Troll Fight" mountainTrollEvent 0 initialStateWithMonster isHpReduced
testMountainTrollUseItemChoice = testEventChoice "Mountain Troll Use Item" mountainTrollEvent 1 initialStateWithHealth isHpIncreased
testMountainTrollFleeChoice = testEventChoice "Mountain Troll Flee" mountainTrollEvent 2 initialState hasExitedEvent

-- Test for Treasure Chest Event (Open Chest)
testTreasureChestOpenChoice = testEventChoice "Treasure Chest Open" treasureChest 0 initialStateWithLowerHp isHpIncreased

-- Tests for Ancient Shrine Event
testAncientShrineOfferStrength = testEventChoice "Ancient Shrine Offer Strength" ancientShrineEncounter 0 initialStateWithAttack isAttackReducedAndSwordIncreased
testAncientShrineMeditate = testEventChoice "Ancient Shrine Meditate" ancientShrineEncounter 1 initialStateWithHealth isShieldIncreased

-- Tests for Mysterious Traveler Event
testGoblinRaiderEventFight = testEventChoice "Goblin Raider Fight" goblinRaiderEvent 0 initialStateWithMonster isHpReduced

-- Tests for Sleep Event
testSleepEventChoice = testEventChoice "Sleep Event Choice" sleepEvent 0 initialStateWithLowerHp isHpIncreasedByTwo

tests :: [(String, Test)]
tests = [ 
            ("testPosInitialization", testPosInitialization),
            ("testHPInitialization", testHPInitialization),
            ("testShieldInitialization", testShieldInitialization),
            ("testSwordInitialization", testSwordInitialization),
            ("testAttackInitialization", testAttackInitialization),
            ("testIChoiceInitialization", testIChoiceInitialization),
            ("testGameOverInitialization", testGameOverInitialization),
            ("testWinnerInitialization", testWinnerInitialization),
            ("testLoserInitialization", testLoserInitialization),
            ("testMountainsInitialization", testMountainsInitialization),
            ("testDisplayLogsInitialization", testDisplayLogsInitialization),
            ("testInBattleInitialization", testInBattleInitialization),
            ("testLogsInitialization", testLogsInitialization),
            ("testMonsterBehavior", testMonsterBehavior),
            ("testBoundaries", testBoundaries),
            ("testGoblinRaiderEncounter", testGoblinRaiderEncounter),
            ("testForestNymphEncounter", testForestNymphEncounter),
            ("testMountainTrollEncounter", testMountainTrollEncounter),
            ("testShadowAssassinEncounter", testShadowAssassinEncounter),
            ("testTreasureChestEncounter", testTreasureChestEncounter),
            ("testAncientShrineEncounter", testAncientShrineEncounter),
            ("testMysteriousTravelerEncounter", testMysteriousTravelerEncounter),
            ("testLostTreasureChestEncounter", testLostTreasureChestEncounter),
            ("testEnchantedLakeEncounter", testEnchantedLakeEncounter),
            ("testAncientLibraryEncounter", testAncientLibraryEncounter),
            ("testFinalConfrontationEncounter", testFinalConfrontationEncounter),
            ("testSleepEventChoice", testSleepEventChoice),
            ("testGoblinRaiderEventFight", testGoblinRaiderEventFight),
            ("testForestNymphFightChoice", testForestNymphFightChoice),
            ("testForestNymphUseItemChoice", testForestNymphUseItemChoice),
            ("testForestNymphFleeChoice", testForestNymphFleeChoice),
            ("testMountainTrollFightChoice", testMountainTrollFightChoice),
            ("testMountainTrollUseItemChoice", testMountainTrollUseItemChoice),
            ("testMountainTrollFleeChoice", testMountainTrollFleeChoice),
            ("testTreasureChestOpenChoice", testTreasureChestOpenChoice),
            ("testAncientShrineOfferStrength", testAncientShrineOfferStrength),
            ("testAncientShrineMeditate", testAncientShrineMeditate)         
        ]
        
-- Main function to run each labeled test and print results
main :: IO ()
main = do
    results <- mapM (runTestTT . snd) tests
    let totalCases = sum $ map cases results
    let totalErrors = sum $ map errors results
    let totalFailures = sum $ map failures results
    let totalSuccesses = totalCases - totalErrors - totalFailures

    -- Print individual test results in the specified format
    mapM_ (\(label, result) -> do
              let successes = cases result - errors result - failures result
              putStrLn $ label ++ ": {cases = " ++ show (cases result) ++
                         ", errors = " ++ show (errors result) ++
                         ", Successes: " ++ show successes ++
                         ", failures = " ++ show (failures result) ++ "}"
          ) (zip (map fst tests) results)

    -- Print summary
    putStrLn "\nSummary:"
    putStrLn $ "Total Cases: " ++ show totalCases
    putStrLn $ "Total Errors: " ++ show totalErrors
    putStrLn $ "Total Failures: " ++ show totalFailures
    putStrLn $ "Total Successes: " ++ show totalSuccesses
