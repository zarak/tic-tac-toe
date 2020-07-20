module Main where

import GameState
import Control.Monad (forever, when)
import System.Exit (exitSuccess)
import System.Random (randomRIO)

handleMove :: GameState Board Player -> Int -> IO (GameState Board Player)
handleMove game idx = return (move game idx)

gameOver :: GameState Board Player -> IO ()
gameOver game
  | game `isWinFor` X = do 
      print game
      putStrLn "Player X wins!" 
      exitSuccess 
  | game `isWinFor` O = do
      print game
      putStrLn "Player O wins!" 
      exitSuccess 
  | isEnd game = do
      print game
      putStrLn "Game tied"
      exitSuccess
  | otherwise = return ()

runGame :: GameState Board Player -> IO ()
runGame game = forever $ do
    gameOver game
    print game
    move <- getLine
    let idx = read move :: Int 
     in case idx `elem` possibleMoves game of 
          True -> handleMove game idx >>= runGame 
          False -> putStrLn "Invalid move"

main :: IO ()
main = do
    runGame initBoard
