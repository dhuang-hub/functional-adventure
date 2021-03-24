module GameIO where

import Control.Monad.State
import System.Exit
import System.IO

import GameState
import Player
import Room
import Command
import Item


-- type alias for GameIO
type GameIO a = StateT GameState IO a


-- function for printing the prompt
prompt :: GameIO ()
prompt = liftIO (putStr "-> " >> hFlush stdout)


printBlank :: GameIO ()
printBlank = liftIO (putStrLn "" >> hFlush stdout)


-- function for printing gamestate message
printMessage :: GameIO ()
printMessage = do
    gm <- get
    let msg = message gm
    if msg == Nothing
        then pure ()
        else liftIO (putStrLn (str msg) >> hFlush stdout)
    where str (Just s) = s


-- function for printing room descriptions
printDescription :: GameIO ()
printDescription = do
    gm <- get
    liftIO (putStrLn (Room.desc . currentRoom $ gm) >> hFlush stdout)


-- function for printing objects in the room
printObjects :: GameIO ()
printObjects = do
    gm <- get
    let objMsg = ["You see the following objects:"] ++ (nearbyObjects gm)
    if length objMsg > 1
        then liftIO (mapM_ putStrLn objMsg >> hFlush stdout)
        else pure ()


-- function for printing the exits in room
printExits :: GameIO ()
printExits = do
    gm <- get
    let exits' = (map $ show . fst) . exits . currentRoom $ gm 
    let exitMsg = ["There are exits in the following directions:"] ++ exits'
    if length exitMsg > 1
        then liftIO (mapM_ putStrLn exitMsg >> hFlush stdout)
        else pure ()


-- function for printing the player's inventory
printInventory :: GameIO ()
printInventory = do
    gm <- get
    let inv = currentInventory gm
    let invMsg = ["You are carrying the following objects:"] ++ inv
    if length invMsg > 1
        then liftIO (mapM_ putStrLn invMsg)
        else let err = "You aren't carrying anything."
            in liftIO (putStrLn err >> hFlush stdout)


-- function for printing helpful player commands
printHelp :: GameIO ()
printHelp = let
    lookHelp = "\"look\" - Prints a description of your surroundings."
    lookAtHelp = "\"look at 'thing1', 'thing2', ...\" - Prints a description\
        \ of an object that's either nearby or in the inventory."
    takeHelp = "\"take 'thing1', 'thing2', ...\" - Take items from nearby and\
        \ add to inventory."
    dropHelp = "\"drop 'thing1', 'thing2', ...\" - Drop items from your\
        \ inventory into the current room. Watch what you drop! You may\
        \ get stuck."
    useHelp = "\"use 'thing1', 'thing2', ...\" - Interact with objects that\
        \ are either nearby or in the inventory."
    dirHelp = "\"north\", \"east\", \"south\", \"west\" - Move in the stated\
        \ direction, if possible."
    helpHelp = "\"help\" - Print out this 'help' anytime!"
    andHelp = "Tip: Stitch commands together with \"and\" between commands,\
        \ e.g. \"look at stairsup, stapler and north\"."
    commandHelp = [ lookHelp, lookAtHelp, takeHelp, dropHelp
                  , useHelp, dirHelp, helpHelp, andHelp ]
    in liftIO (mapM_ putStrLn commandHelp >> hFlush stdout)



-- effect change for GameIO
effectChange :: (GameState -> GameState) -> GameIO ()
effectChange f = do
    gm <- get
    put $ f gm
    pure ()


-- function for executing a list of actions
actionOverList :: (ItemName -> GameState -> GameState)
               -> [ItemName]
               -> GameIO ()
actionOverList _ [] = pure ()
actionOverList f (x : xs) = effectChange (f x) >> printMessage
                            >> actionOverList f xs


-- function for printing a finished game 
finishGame :: GameIO ()
finishGame = liftIO (mapM_ putStrLn finMsg >> exitSuccess)
    where finMsg = [ "You successfully completed Functional Advencture!"
                   , "Congrats! You've managed to escape Crerar Library!"
                   , "But more importantly, you've learned Haskell!" ]

-- function for user exit
exit :: IO ()
exit = putStrLn "Goodbye!" >> exitSuccess


-- function for checking a game-winning state
checkGameOver :: GameIO ()
checkGameOver = do
    gm <- get
    if haveWonGame gm == True
        then finishGame
        else pure ()


-- function for printout of response to erroneous commands
syntaxError :: GameIO ()
syntaxError = liftIO (putStrLn "I don't understand that.")


-- function for executing actions of commands
performCommand :: Command -> GameIO ()
performCommand Look = printDescription >> printBlank >>
                      printObjects >> printBlank >> printExits
performCommand (Move d) = do
    gm <- get
    put $ move d gm
    printMessage
performCommand (Take ilist) = actionOverList takeItem ilist
performCommand (Drop ilist) = actionOverList dropItem ilist
performCommand Inventory = printInventory
performCommand Exit = liftIO exit
performCommand (LookAt ilist) = actionOverList lookAtItem ilist
performCommand (Use ilist) = actionOverList useItem ilist
performCommand Help = printHelp


-- function for performing commands of a conjunction
performConjunction :: Conjunction -> GameIO ()
performConjunction [] = pure ()
performConjunction (x : xs) = performCommand x >> performConjunction xs


-- function for parsing input conjunction string
parseConjunction :: String -> GameIO ()
parseConjunction s = case parse s of
    (Just c) -> performConjunction c
    otherwise -> syntaxError


-- function for a round of game in the repl
repl :: GameIO ()
repl = do
    prompt
    str <- liftIO getLine
    parseConjunction str
    checkGameOver