module Main where

import Data.Char
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

validate :: String -> Maybe Int
validate s =
    if (not . null) s && (isDigit . head $ s)
       then Just (digitToInt . head $ s)
       else Nothing

runGame :: GameState Board Player -> IO ()
runGame game = forever $ do
    gameOver game
    print game
    putStrLn "Enter move> "
    move <- getLine
    let idx = validate move
     in case idx of 
          Just idx -> if idx `elem` possibleMoves game then handleMove game idx >>= runGame else putStrLn "Invalid move"
          Nothing -> putStrLn "Invalid move"

main :: IO ()
main = do
    runGame initBoard
