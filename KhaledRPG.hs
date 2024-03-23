import System.IO
import System.Random
import System.Directory (doesFileExist)
import Control.Exception (tryJust)
import Control.Monad (guard, when)
import Data.Char (toUpper, isSpace)  -- Import isSpace
import Text.Read (readMaybe)
import System.IO.Error (isDoesNotExistError)
import System.Exit (exitSuccess)  -- Import exitSuccess

data Player = Player
    { playerName :: String
    , playerHealth :: Int
    , playerAttack :: Int
    , playerDefense :: Int
    , defending :: Bool
    , playerExperience :: Int
    , playerLevel :: Int
    , campfireUses :: Int
    }

data Enemy = Enemy
    { enemyName :: String
    , enemyHealth :: Int
    , enemyAttack :: Int
    , enemyDefense :: Int
    , dropsHealingItem :: Bool
    , enemyDropExperience :: Int
    , enemyLevel :: Int
    }

instance Show Player where
    show (Player name health attack defense defending experience level campfireUses) =
        unwords [name, show health, show attack, show defense, show defending, show experience, show level, show campfireUses]

instance Read Player where
    readsPrec _ input =
        case words input of
            [name, health, attack, defense, defending, experience, level, campfireUses] ->
                [(Player name (read health) (read attack) (read defense) (read defending) (read experience) (read level) (read campfireUses), "")]
            _ -> []

-- Save Player's Data to File
saveGame :: Player -> FilePath -> IO ()
saveGame player saveFileName = do
    putStrLn "Saving game..."
    writeFile (playerName player ++ ".txt") (show player)
    putStrLn "Game saved successfully."

-- Load Player's Data from File
loadGame :: String -> IO (Maybe Player)
loadGame playerName = do
    let fileName = playerName ++ ".txt"
    fileExists <- doesFileExist fileName
    if fileExists
        then do
            contents <- tryJust (guard . isDoesNotExistError) $ readFile fileName
            case contents of
                Left _ -> do
                    putStrLn $ "Failed to load game for " ++ playerName ++ ". Starting new game instead."
                    return Nothing
                Right contentStr -> do
                    case readMaybe contentStr :: Maybe Player of
                        Just player -> do
                            putStrLn $ "Loaded game for " ++ playerName ++ "."
                            return (Just player)
                        Nothing -> do
                            putStrLn $ "Failed to parse game data for " ++ playerName ++ ". Starting new game instead."
                            return Nothing
        else do
            putStrLn $ "No saved game found for " ++ playerName ++ ". Starting new game."
            return Nothing

-- Modify main function
main :: IO ()
main = do
    putStrLn "Welcome to Khaled RPG!"
    putStrLn "Enter your character's name:"
    name <- getLine
    existingPlayer <- loadGame name
    case existingPlayer of
        Just player -> do
            putStrLn "Do you want to start a new game (N) or continue from the existing saved game (C)?"
            choice <- getLine
            when (map toUpper choice == "N") $ do
                startNewGame name name
            when (map toUpper choice == "C") $ do
                putStrLn $ "Welcome back, " ++ playerName player ++ "!"
                putStrLn "Let's continue our adventure..."
                explore player name
        Nothing -> do
            startNewGame name name

-- Function to start a new game
startNewGame :: String -> FilePath -> IO ()
startNewGame name saveFileName = do
    let player = createPlayer name
    putStrLn $ "Welcome, " ++ playerName player ++ "!"
    putStrLn "Let's embark on an adventure..."
    explore player saveFileName

-- Check if the entered name is valid
isValidName :: String -> Bool
isValidName name = not (null (trim name))

-- Trim leading and trailing spaces
trim :: String -> String
trim = reverse . dropWhile isSpace . reverse . dropWhile isSpace

-- Create a new player
createPlayer :: String -> Player
createPlayer name = Player
    { playerName = name
    , playerHealth = 100
    , playerAttack = 20
    , playerDefense = 3
    , defending = False
    , playerExperience = 0
    , playerLevel = 1
    , campfireUses = 3  -- Start with 3 campfire uses
    }

-- Explore different locations
explore :: Player -> FilePath -> IO ()
explore player saveFileName = do
    putStrLn "You are in a forest."
    putStrLn "1. Look for enemies"
    putStrLn "2. Use campfire"
    putStrLn "3. Save game"
    putStrLn "4. Show player stats"
    putStrLn "5. Quit"
    choice <- getLine
    case choice of
        "1" -> do
            enemy <- generateEnemy
            if enemyLevel enemy > playerLevel player
                then do
                    putStrLn "The enemy's level is higher than yours."
                    putStrLn "You should level up or look for another enemy."
                    explore player saveFileName
                else do
                    putStrLn $ "You encountered a " ++ enemyName enemy ++ "!"
                    battle player enemy saveFileName
        "2" -> do
            if campfireUses player > 0
                then do
                    player' <- useCampfire player
                    explore player' saveFileName
                else do
                    putStrLn "You have no more uses of the campfire left."
                    explore player saveFileName
        "3" -> do
            saveGame player saveFileName
            explore player saveFileName
        "4" -> do
            showPlayerStats player
            explore player saveFileName
        "5" -> do
            putStrLn "Goodbye!"
            exitSuccess
        _ -> do
            putStrLn "Invalid choice."
            explore player saveFileName

-- Battle sequence
battle :: Player -> Enemy -> FilePath -> IO ()
battle player enemy saveFileName = do
    putStrLn "Battle begins!"
    battleLoop player enemy saveFileName

battleLoop :: Player -> Enemy -> FilePath -> IO ()
battleLoop player enemy saveFileName
    | playerHealth player <= 0 = putStrLn "You were defeated!"
    | enemyHealth enemy <= 0 = do
        putStrLn $ "You defeated the " ++ enemyName enemy ++ "!"
        let player' = player { playerExperience = playerExperience player + enemyDropExperience enemy }
        let player'' = if playerExperience player' >= 100
                          then player' { playerExperience = playerExperience player' - 100, playerLevel = playerLevel player' + 1 }
                          else player'
        let player''' = player'' { campfireUses = min 3 (campfireUses player'' + 2) } -- Increment campfire uses by 2, but limit to maximum of 3
        explore player''' saveFileName
    | otherwise = do
        putStrLn $ "Your health: " ++ show (playerHealth player)
        putStrLn $ "Enemy health: " ++ show (enemyHealth enemy)
        putStrLn "1. Attack"
        putStrLn "2. Defend"
        choice <- getLine
        case choice of
            "1" -> do
                let (enemy', player') = attack player enemy
                let (player'', enemy'') = enemyAttackLoop player' enemy'
                battleLoop player'' enemy'' saveFileName
            "2" -> do
                let player' = player { defending = True }
                let (player'', enemy') = enemyAttackLoop player' enemy
                let player''' = player'' { defending = False }  -- Reset defending flag to False after enemy's attack
                battleLoop player''' enemy' saveFileName
            _ -> do
                putStrLn "Invalid choice."
                battleLoop player enemy saveFileName

-- Player attacks the enemy
attack :: Player -> Enemy -> (Enemy, Player)
attack player enemy =
    if damage > 0
        then (enemy { enemyHealth = newEnemyHealth }, player)
        else (enemy, player)
    where
        damage = max 0 (playerAttack player - enemyDefense enemy)
        newEnemyHealth = max 0 (enemyHealth enemy - damage)

-- Enemy attacks the player
enemyAttackLoop :: Player -> Enemy -> (Player, Enemy)
enemyAttackLoop player enemy =
    if damage > 0
        then (player { playerHealth = newPlayerHealth }, enemy)
        else (player, enemy)
    where
        damage = if defending player
                     then max 0 ((enemyAttack enemy `div` 2) - playerDefense player)  -- If defending, reduce enemy's attack by half
                     else max 0 (enemyAttack enemy - playerDefense player)
        newPlayerHealth = max 0 (playerHealth player - damage)

-- Generate random enemies
generateEnemy :: IO Enemy
generateEnemy = do
    let enemies = [Enemy "AnotherOne" 30 6 3 False 30 1 , Enemy "GODDID" 43 8 4 True 50 2, Enemy "DJ Khaled" 60 11 7 False 100 5]
    randomIndex <- randomRIO (0, length enemies - 1)
    return $ enemies !! randomIndex

-- Explore with healing item option
exploreWithHealing :: Player -> FilePath -> IO ()
exploreWithHealing player saveFileName = do
    putStrLn "1. Use healing item"
    putStrLn "2. Continue exploring"
    choice <- getLine
    case choice of
        "1" -> do
            let player' = player { playerHealth = 100 }  -- Restore player's health to full (100)
            putStrLn "You used the healing item."
            explore player' saveFileName
        "2" -> explore player saveFileName
        _ -> do
            putStrLn "Invalid choice."
            exploreWithHealing player saveFileName

-- Use campfire to heal player
useCampfire :: Player -> IO Player
useCampfire player = do
    randomEvent <- generateCampfireEvent
    case randomEvent of
        HealingEvent healAmount -> do
            let newHealth = min 100 (playerHealth player + healAmount)
            putStrLn $ "You feel rejuvenated by the warmth of the campfire. Your health is now " ++ show newHealth ++ " hp."
            return player { playerHealth = newHealth, campfireUses = campfireUses player - 1 }  -- Decrement campfire usage
        NoHealEvent message -> do
            putStrLn message
            return player

data CampfireEvent
    = HealingEvent Int  -- Player heals normally with a given amount
    | NoHealEvent String  -- Player cannot heal due to certain circumstances
    deriving (Show)

-- Generate a random campfire event
generateCampfireEvent :: IO CampfireEvent
generateCampfireEvent = do
    randomNumber <- randomRIO (1 :: Int, 20 :: Int)
    case randomNumber of
        1 -> return $ NoHealEvent "While sitting by the campfire, you hear strange noises nearby. You cannot concentrate enough to heal."
        2 -> return $ NoHealEvent "You notice dark clouds gathering above. It's not a good time to rest."
        3 -> return $ NoHealEvent "A sudden gust of wind extinguishes the campfire. You need to relight it before you can rest."
        4 -> return $ NoHealEvent "You feel a sudden chill in the air, making it difficult to warm yourself by the campfire."
        5 -> return $ NoHealEvent "You hear howling in the distance, making you uneasy and unable to relax."
        6 -> return $ NoHealEvent "A group of insects swarms around the campfire, causing irritation and preventing you from resting."
        7 -> return $ NoHealEvent "You accidentally burn your hand while tending to the campfire, making it painful to sit close."
        8 -> return $ NoHealEvent "A nearby animal approaches your campsite, causing you to stay alert and unable to rest."
        _ -> do
            randomHeal <- randomRIO (15, 20)  -- Generate random healing value between 15 and 20
            return $ HealingEvent randomHeal

-- Function to show player stats
showPlayerStats :: Player -> IO ()
showPlayerStats player = do
    putStrLn $ "Player Name: " ++ playerName player
    putStrLn $ "Health: " ++ show (playerHealth player)
    putStrLn $ "Attack: " ++ show (playerAttack player)
    putStrLn $ "Defense: " ++ show (playerDefense player)
    putStrLn $ "Experience: " ++ show (playerExperience player)
    putStrLn $ "Level: " ++ show (playerLevel player)