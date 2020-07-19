module GameState where

import Data.List
import Data.List.Split
import Data.Function

data Player 
    = X 
    | O 
    deriving (Show, Eq, Enum)

type Turn = Player

newtype Board
  = Board String
  deriving Eq

instance Show Board where
    show = render

data GameState board turn
    = GameState board turn 
    deriving Eq

instance (Show board, Show turn) => Show (GameState board turn) where
    show (GameState board turn) = "\n" ++ show board ++ "\nTurn: " ++ show turn

dim = 3
size = dim^2

initBoard :: GameState Board Turn
initBoard = GameState (Board $ replicate size ' ') X 

render :: Board -> String
render (Board board) =
    unlines .
    intersperse (replicate 11 '-') .
    map (intercalate "|") .
    chunksOf dim .
    map (\(i, c) -> " " ++ (if c == ' ' then show i else [c]) ++ " ") $ 
    zip [0..] board
