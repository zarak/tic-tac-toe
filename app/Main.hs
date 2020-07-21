module Main where

import Data.Char
import GameState
import Control.Monad (forever, when)
import System.Exit (exitSuccess)
import System.Random (randomRIO)

handleMove :: GameState Board Player -> IO (GameState Board Player)
--handleMove game idx = return (move game idx)
handleMove game = do
    putStrLn "Enter move> "
    selectedMove <- getLine
    let idx = validate selectedMove
     in case idx of 
          Just idx -> if idx `elem` possibleMoves game
                         then return (move game idx)
                         else do
                            putStrLn "Invalid move"
                            return game
                         --else putStrLn "Invalid move"
          Nothing -> do
              putStrLn "Invalid move"
              return game

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
    handleMove game >>= runGame 

main :: IO ()
main = runGame initBoard
