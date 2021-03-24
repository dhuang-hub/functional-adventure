module Main where

import Control.Monad.State
import Control.Exception
import System.IO

import Initialize
import GameIO


controlD :: IOError -> IO ()
controlD eofErrorType = exit

main :: IO ()
main = do
    putStrLn "Welcome to Functional Adventure!"
    putStrLn "You wake up in a daze and you have absolutely\
             \ no clue where you are."
    putStrLn "USE \"help\" for tips and help on commands."
    putStrLn "Good Luck!"
    handle controlD $ evalStateT (forever repl) initialState

