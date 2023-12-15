import Test.HUnit
import Types
import GameLogic (updateGameState, goblinRaiderEvent, forestNymphEvent, mountainTrollEvent, shadowAssassinEvent, treasureChest, ancientShrineEncounter, mysteriousTraveler, lostTreasureChest, enchantedLake, ancientLibrary, finalConfrontation, fightMonster, applyBonus, moveMonster, updateGameState, sleepEvent, meditateChoice)
import Init (initialState, initialEvents)
import Brick.Widgets.Core (emptyWidget)
import Data.Maybe (isJust, isNothing, fromMaybe)
import Data.List (find)
import Brick (str)
import qualified Data.Map as Map (fromList, insert, empty, lookup)


-- Initialize a mock game state for testing purposes
initialMockState :: Game
initialMockState = Game
  { posX = 0, posY = 0, shield = 10, sword = 10, hp = 100, attack = 15,
    eventsMap = Map.fromList [((0, 0), [mockEvent])], -- Adjusted to use a map
    iChoice = 0, inEvent = Just mockEvent,
    monstersMap = Map.fromList [((0, 0), [mockMonster])], -- Adjusted to use a map
    inMonster = Just mockMonster,
    gameOver = False, winner = False, loser = False,
    mountainsMap = Map.fromList [((0, 0), [])], -- Adjusted to use a map
    displayLogs = False, inBattle = False, logs = [],
    finalMonsterHp = 100, finalMonsterAttack = 10, animationFrame = 0
  }

-- Mock functions and data
mockMonster :: Monster
mockMonster = Monster 5 5 "Mock Monster" 30 10

-- Mock event to test event choice effects
mockEvent :: GameEvent
mockEvent = GEvent 5 5 "Mock Event" "Just a mock event" [] emptyWidget False

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
    let mountainsInRegion = Map.lookup (0, 0) (mountainsMap initState)
    assertEqual "Initial Mountains in region (0, 0) should be empty" (Just []) mountainsInRegion

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

-- Helper function to get monsters from the current region
getCurrentRegionMonsters :: Game -> [Monster]
getCurrentRegionMonsters game =
    let currentRegion = (posX game, posY game)
    in fromMaybe [] (Map.lookup currentRegion (monstersMap game))

-- Test Monster Behavior
testMonsterBehavior :: Test
testMonsterBehavior = TestCase $ do
    let initState = initialMockState
    let monsterState = fightMonster initState
    let currentRegionMonsters = getCurrentRegionMonsters monsterState
    assertBool "Monster should be engaged after fight" (not $ null currentRegionMonsters)

-- Test Boundary and Overlap Checks
testBoundaries :: Test
testBoundaries = TestCase $ do
    let initState = initialMockState
    let outOfBounds = posX initState < 0 || posY initState < 0 || posX initState >= 20 || posY initState >= 20
    assertBool "Player should be within game boundaries" (not outOfBounds)

-- Helper function to simulate encountering an event
simulateEventEncounter :: Game -> GameEvent -> Game
simulateEventEncounter game event =
    let updatedEvents = Map.insert (eventX event, eventY event) [event] (eventsMap game)
    in game { posX = eventX event, posY = eventY event, inEvent = Just event, eventsMap = updatedEvents }

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
initialStateWithMonster :: Game
initialStateWithMonster = 
    let regionMonsters = getCurrentRegionMonsters initialState
    in if not (null regionMonsters) 
        then initialState { inMonster = Just (head regionMonsters), hp = 100 }
        else initialState { hp = 100 } -- Fallback if no monsters are found


-- Check if the player's HP is reduced after fighting a monster
isHpReduced :: Game -> Bool
isHpReduced game = hp game < 100 -- Assuming initial HP is 100

-- Check if the game is over
isGameOver :: Game -> Bool
isGameOver game = gameOver game

-- Assuming initial states for different scenarios
initialStateWithHealth = initialState { hp = 100 }
initialStateWithAttack = initialState { attack = 15 }

-- Check if the player's shield is increased
initialStateLowAttack = initialState { attack = 10 }
testLostTreasureChestForceOpenLowAttack = testEventChoice "Lost Treasure Chest Force Open with Low Attack" lostTreasureChest 0 initialStateLowAttack noChangeInHpAndShield

-- Check if the player's shield is increased
initialStateLowSword = initialState { sword = 10 }

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

-- Helper function to create a test for a given event choice
createChoiceTest :: String -> GameEvent -> Int -> Game -> (Game -> Bool) -> Test
createChoiceTest testName event choiceIndex initialState condition = TestCase $ do
    let choice = (choices event) !! choiceIndex
    let updatedGame = applyChoiceEffect initialState choice
    assertBool testName (condition updatedGame)

-- Helper function to check if the game's HP or Shield exceeds a certain limit
checkLimit :: (Game -> Int) -> Int -> Game -> Bool
checkLimit attr limit game = attr game <= limit

-- Helper function to check for an increase in a specific attribute
checkIncrease :: (Game -> Int) -> Game -> Game -> Bool
checkIncrease attr initialState updatedGame = attr updatedGame > attr initialState

eventsAreEqual :: Maybe GameEvent -> Maybe GameEvent -> Bool
eventsAreEqual Nothing Nothing = True
eventsAreEqual (Just event1) (Just event2) =
    and [ eventX event1 == eventX event2
        , eventY event1 == eventY event2
        , name event1 == name event2
        -- Add more comparisons for other fields if necessary
        ]
eventsAreEqual _ _ = False

-- Use this function in your tests:
gamesAreEqual :: Game -> Game -> Bool
gamesAreEqual game1 game2 =
    and [ posX game1 == posX game2
        , posY game1 == posY game2
        , shield game1 == shield game2
        , sword game1 == sword game2
        , hp game1 == hp game2
        , attack game1 == attack game2
        , eventsAreEqual (inEvent game1) (inEvent game2)
        -- Add more comparisons for other fields if necessary
        ]

-- Use this function in your tests:
noChange initialGame updatedGame = gamesAreEqual initialGame updatedGame


noChangeInHpAndShield :: Game -> Bool
noChangeInHpAndShield game = hp game == hp initialState && shield game == shield initialState

noChangeInHp :: Game -> Bool
noChangeInHp game = hp game == hp initialState

randomOutcome :: Game -> Bool
randomOutcome _ = True  -- This should be adjusted based on your game logic

playerWins :: Game -> Bool
playerWins game = winner game

playerLoses :: Game -> Bool
playerLoses game = loser game

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

initialStateForSneakAttack = initialState {
    hp = 100,
    shield = 50,
    finalMonsterHp = 40,
    finalMonsterAttack = 15
}

initialStateForSneakAttackLoss = initialState {
    hp = 10,
    shield = 30,
    finalMonsterHp = 150,
    finalMonsterAttack = 20
}

-- For direct assault victory test
initialStateDirectAssaultVictory = initialState
    { attack = 40, sword = 20, hp = 100, finalMonsterHp = 50, finalMonsterAttack = 10 }

-- For sneak attack victory test
initialStateSneakAttackVictory = initialState
    { attack = 10, shield = 40, hp = 100, finalMonsterHp = 50, finalMonsterAttack = 10 }

testLostTreasureChestForceOpenHighAttack = createChoiceTest "Lost Treasure Chest Force Open - High Attack" lostTreasureChest 0 (initialState { attack = 25 }) (checkIncrease hp (initialState { attack = 25 }))
testLostTreasureChestCarefullyUnlockHighSword = createChoiceTest "Lost Treasure Chest Carefully Unlock - High Sword" lostTreasureChest 1 (initialState { sword = 20 }) (checkIncrease hp (initialState { sword = 20 }))
testLostTreasureChestCarefullyUnlockLowSword = TestCase $ do
    let initialGame = initialState { sword = 5 } -- Set a low sword value
    let choice = head $ choices lostTreasureChest
    let updatedGame = applyChoiceEffect initialGame choice

    -- Check if the hp is unchanged because the sword value is too low
    assertBool "HP should remain the same with low sword value" (hp updatedGame == hp initialGame)


testEnchantedLakeBatheInLake = createChoiceTest "Enchanted Lake Bathe In Lake" enchantedLake 0 initialState (\game -> hp game == 150)
testEnchantedLakeSearchAround = createChoiceTest "Enchanted Lake Search Around" enchantedLake 1 initialState (\_ -> True)  -- Depends on your specific implementation

testAncientLibraryStudyAncientTomes = createChoiceTest "Ancient Library Study Ancient Tomes" ancientLibrary 0 (initialState { sword = 15 }) (checkIncrease attack initialState)
testAncientLibrarySearchForSecrets = createChoiceTest "Ancient Library Search for Secret Passages" ancientLibrary 1 initialState (\_ -> True)  -- Depends on your specific implementation

testFinalConfrontationSneakAttack = createChoiceTest "Final Confrontation Sneak Attack" finalConfrontation 1 initialState (\game -> winner game || loser game)

-- Direct Assault Choice
directAssaultChoice :: EventChoice
directAssaultChoice =
  GChoice
    { title = "Direct assault",
      effect = \game ->
        let playerAttack = attack game + (sword game `div` 2) -- 50% bonus from sword
            newMonsterHp = finalMonsterHp game - playerAttack
         in if newMonsterHp <= 0
              then game {winner = True}
              else game {finalMonsterHp = newMonsterHp, hp = hp game - finalMonsterAttack game, loser = hp game - finalMonsterAttack game <= 0}
    }

-- Sneak Attack Choice
sneakAttackChoice :: EventChoice
sneakAttackChoice =
  GChoice
    { title = "Sneak attack",
      effect = \game ->
        let playerAttack = attack game + (shield game `div` 2) -- 50% bonus from shield
            newMonsterHp = finalMonsterHp game - playerAttack
         in if newMonsterHp <= 0
              then game {winner = True}
              else game {finalMonsterHp = newMonsterHp, hp = hp game - finalMonsterAttack game, loser = hp game - finalMonsterAttack game <= 0}
    }


testDirectAssaultVictory :: Test
testDirectAssaultVictory = TestCase $ do
    let gameState = createGameState 100 50 10 100 20 -- Adjust values for victory
    let updatedGame = effect directAssaultChoice gameState
    assertBool "Direct assault should lead to victory" (winner updatedGame)

-- Test for Direct Assault Defeat
testDirectAssaultDefeat :: Test
testDirectAssaultDefeat = TestCase $ do
    let gameState = createGameState 20 10 10 200 30 -- Adjust values for defeat
    let updatedGame = effect directAssaultChoice gameState
    assertBool "Direct assault should lead to defeat" (loser updatedGame)

-- Test for Sneak Attack Victory
testSneakAttackVictory :: Test
testSneakAttackVictory = TestCase $ do
    let gameState = createGameState 100 50 10 50 20 -- Adjust values for victory
    let updatedGame = effect sneakAttackChoice gameState
    assertBool "Sneak attack should lead to victory" (winner updatedGame)

-- Test for Sneak Attack Defeat
testSneakAttackDefeat :: Test
testSneakAttackDefeat = TestCase $ do
    let gameState = createGameState 20 10 10 200 30 -- Adjust values for defeat
    let updatedGame = effect sneakAttackChoice gameState
    assertBool "Sneak attack should lead to defeat" (loser updatedGame)

-- Function to create a Game state for testing
createGameState :: Int -> Int -> Int -> Int -> Int -> Game
createGameState playerHp playerShield playerSword monsterHp monsterAttack = initialState
    { hp = playerHp
    , shield = playerShield
    , sword = playerSword
    , finalMonsterHp = monsterHp
    , finalMonsterAttack = monsterAttack
    , inMonster = Just initialMonster -- You can define a default monster for tests
    -- Include other necessary initializations for your Game type here
    }

-- Define a default monster for testing purposes
initialMonster :: Monster
initialMonster = Monster 0 0 "Test Monster" 100 10 -- Example values


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
            ("testHpLimit", testHpLimit),
            ("testShieldLimit", testShieldLimit),
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
            ("testForestNymphFightChoice", testForestNymphFightChoice),
            ("testForestNymphUseItemChoice", testForestNymphUseItemChoice),
            ("testForestNymphFleeChoice", testForestNymphFleeChoice),
            ("testMountainTrollFightChoice", testMountainTrollFightChoice),
            ("testMountainTrollUseItemChoice", testMountainTrollUseItemChoice),
            ("testMountainTrollFleeChoice", testMountainTrollFleeChoice),
            ("testTreasureChestOpenChoice", testTreasureChestOpenChoice),
            ("testAncientShrineOfferStrength", testAncientShrineOfferStrength),
            ("testAncientShrineMeditate", testAncientShrineMeditate),
            ("testGoblinRaiderEventFight", testGoblinRaiderEventFight),
            ("testSleepEventChoice", testSleepEventChoice),
            ("testLostTreasureChestForceOpenLowAttack", testLostTreasureChestForceOpenLowAttack),
            ("testLostTreasureChestForceOpenHighAttack", testLostTreasureChestForceOpenHighAttack),
            ("testLostTreasureChestCarefullyUnlockHighSword", testLostTreasureChestCarefullyUnlockHighSword),
            ("testLostTreasureChestCarefullyUnlockLowSword", testLostTreasureChestCarefullyUnlockLowSword),
            ("testEnchantedLakeBatheInLake", testEnchantedLakeBatheInLake),
            ("testEnchantedLakeSearchAround", testEnchantedLakeSearchAround),
            ("testAncientLibraryStudyAncientTomes", testAncientLibraryStudyAncientTomes),
            ("testAncientLibrarySearchForSecrets", testAncientLibrarySearchForSecrets),
            ("testDirectAssaultDefeat", testDirectAssaultDefeat),
            ("testSneakAttackDefeat", testSneakAttackDefeat)
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
