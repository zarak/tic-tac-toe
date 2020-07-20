module Main where

import GameState
import Control.Monad (forever, when)
import System.Exit (exitSuccess)
import System.Random (randomRIO)

handleMove :: GameState Board Player -> Int -> IO (GameState Board Player)
handleMove game idx = do return (move game idx)

gameOver :: GameState Board Player -> IO ()
gameOver game =
    when (isEnd game) do 
        putStrLn "Game over!"
        exitSuccess

runGame :: GameState Board Player -> IO ()
runGame game = forever $ do
    gameOver game
    print game
    move <- getLine
    handleMove game (read move :: Int) >>= runGame

main :: IO ()
main = do
    runGame initBoard
