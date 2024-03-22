import System.IO
import System.Random
import Data.Char (isSpace)


data Player = Player
    { playerName :: String
    , playerHealth :: Int
    , playerAttack :: Int
    , playerDefense :: Int
    , defending :: Bool
    }

data Enemy = Enemy
    { enemyName :: String
    , enemyHealth :: Int
    , enemyAttack :: Int
    , enemyDefense :: Int
    , dropsHealingItem :: Bool
    }

-- Game initialization

main :: IO ()
main = do
    putStrLn "Welcome to Khaled RPG!"
    putStrLn "Enter your character's name:"
    name <- getLine
    if isValidName name
        then do
            let player = createPlayer name
            putStrLn $ "Welcome, " ++ playerName player ++ "!"
            putStrLn "Let's embark on an adventure..."
            explore player
        else do
            putStrLn "Invalid name. Please enter a valid name."
            main

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
    , defending = False  -- Initially not defending
    }

-- Explore different locations

explore :: Player -> IO ()
explore player = do
    putStrLn "You are in a forest."
    putStrLn "1. Look for enemies"
    putStrLn "2. Use campfire"
    putStrLn "3. Quit"
    choice <- getLine
    case choice of
        "1" -> do
            enemy <- generateEnemy
            putStrLn $ "You encountered a " ++ enemyName enemy ++ "!"
            battle player enemy
        "2" -> do
            player' <- useCampfire player
            putStrLn "You feel rejuvenated by the warmth of the campfire."
            putStrLn $ "Your health is " ++ show (playerHealth player') ++ " hp."
            explore player'
        "3" -> putStrLn "Goodbye!"
        _ -> do
            putStrLn "Invalid choice."
            explore player


-- Battle sequence

battle :: Player -> Enemy -> IO ()
battle player enemy = do
    putStrLn "Battle begins!"
    battleLoop player enemy

battleLoop :: Player -> Enemy -> IO ()
battleLoop player enemy
    | playerHealth player <= 0 = putStrLn "You were defeated!"
    | enemyHealth enemy <= 0 = do
        putStrLn $ "You defeated the " ++ enemyName enemy ++ "!"
        if dropsHealingItem enemy
            then do
                putStrLn "The enemy dropped a healing item!"
                exploreWithHealing player
            else explore player
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
                battleLoop player'' enemy''
            "2" -> do
                let player' = player { defending = True }  -- Set defending flag to True
                let (player'', enemy') = enemyAttackLoop player' enemy
                let player''' = player'' { defending = False }  -- Reset defending flag to False after enemy's attack
                battleLoop player''' enemy'
            _ -> do
                putStrLn "Invalid choice."
                battleLoop player enemy

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
    let enemies = [Enemy "AnotherOne" 30 6 3 False, Enemy "GODDID" 43 8 4 True, Enemy "DJ Khaled" 60 11 7 False]
    randomIndex <- randomRIO (0, length enemies - 1)
    return $ enemies !! randomIndex

-- Explore with healing item option

exploreWithHealing :: Player -> IO ()
exploreWithHealing player = do
    putStrLn "1. Use healing item"
    putStrLn "2. Continue exploring"
    choice <- getLine
    case choice of
        "1" -> do
            let player' = player { playerHealth = 100 }  -- Restore player's health to full (100)
            putStrLn "You used the healing item."
            explore player'
        "2" -> explore player
        _ -> do
            putStrLn "Invalid choice."
            exploreWithHealing player

-- Use campfire to heal player

useCampfire :: Player -> IO Player
useCampfire player = do
    randomHeal <- randomRIO (15, 20)  -- Generate random healing value between 15 and 20
    putStrLn $ "Random healing value: " ++ show randomHeal
    let newHealth = min 100 (playerHealth player + randomHeal)  -- Cap health at 100
    return player { playerHealth = newHealth }