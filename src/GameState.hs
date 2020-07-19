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
  deriving (Eq, Show)

data GameState 
    = GameState Board Turn 
    deriving (Eq, Show)

dim = 3
size = dim^2

initBoard :: GameState
initBoard = GameState (Board $ replicate size ' ') X 

render :: GameState -> String
render (GameState (Board board) turn) = undefined
