 {-|
Module      : GameState
Description : Module with pure code for tic-tac-toe
Copyright   : (c) Zarak Mahmud, 2020
License     : GPL-3
Maintainer  : zarak@hotmail.ca
Stability   : experimental
Portability : POSIX

Module containing data structures to represent the game state,
actions to modify state, and rendering functions to display
state.
-}
module GameState where

import Text.Paint
import Data.List
import Data.List.Split
import Data.Function

data Player 
    = X                         -- ^ Player X
    | O                         -- ^ Player O
    deriving (Show, Eq, Enum)

type Turn = Player

newtype Board
  = Board String
  deriving Eq

-- |A board is displayed in the terminal
--  wih the following color scheme:
--  red 'X', blue 'O', and white for everything else.
instance Show Board where
    show = colorize

data GameState board turn
    -- |'GameState' accepts polymorphic types so that the board
    --  and/or turn can also be represented as alternative
    --  data structures (such as a string, or a list of 'X' for board)
    = GameState board turn 
    deriving Eq

-- |The GameState displays the board along with the current turn.
instance (Show board, Show turn) => Show (GameState board turn) where
    show (GameState board turn) = "\n" ++ show board ++ "\nTurn: " ++ show turn

-- |The length of the side of the board - default value is 3.
-- Denoted as \( d \) within this documentation.
dim = 3
-- |The number of cells in the board - \( s = d^2 \).
size = dim^2

-- |The color of 'X'.
red = Paint Maroon Default []
-- |The color of 'O'.
blue = Paint Blue Default []

-- |The state of the game upon initialization.
initBoard :: GameState Board Turn
initBoard = GameState (Board $ replicate size ' ') X 

-- |Color 'X' red and 'O' blue in the terminal.
colorize :: Board -> String
colorize board = concatMap applyColor (render board)
    where applyColor c
            | c == 'X' = paint red [c]
            | c == 'O' = paint blue [c]
            | otherwise = [c]

-- |Display the board as a user-friendly string.
--
-- >>> render (Board "XOX OO X ")
--  0 | 1 | 2
-- -----------
--  3 | 4 | 5
-- -----------
--  6 | 7 | 8
render :: Board -> String
render (Board board) =
    unlines .
    intersperse (replicate 11 '-') .
    map (intercalate "|") .
    chunksOf dim .
    map (\(i, c) -> " " ++ (if c == ' ' then show i else [c]) ++ " ") $ 
    zip [0..] board

-- |Alternate turns between 'X' and 'O'
--
-- >>> switchTurn X
-- O
switchTurn :: Turn -> Turn
switchTurn X = O
switchTurn O = X

-- |'move' marks the desired position with the symbol corresponding
--  to the current player.
move :: GameState Board Turn -> Int -> GameState Board Turn
move (GameState (Board board) turn) idx =
    let newBoard = zipWith (\i c -> if i == idx then (head . show) turn else c) [0..] board
    in
    GameState (Board newBoard) (switchTurn turn)

-- |'possibleMoves' returns a list of the remaining valid moves.
--
-- >>> possibleMoves (GameState (Board "XXXOOOXX ") O)
-- [8]
possibleMoves :: GameState Board Turn -> [Int]
possibleMoves (GameState (Board board) turn) =
    map fst . filter ((==' ') . snd) $ zip [0..] board

-- |Determine whether the game board is empty.
isStart :: GameState Board Turn -> Bool
isStart (GameState (Board board) _) = all (==' ') board

-- |Determine whether 'Player' has achieved the victory condition.
--
-- The main diagonal is the sequence \( \{ 0, d+1, \cdots, s-1 \} \).
-- The off diagonal is the sequence \( \{ d-1, 2d-2, \cdots, s-d \} \).
isWinFor :: GameState Board Turn -> Player -> Bool
isWinFor (GameState (Board board) _) player =
    any consecutive rows 
    || any consecutive (transpose rows) 
    || all ((==(head . show) player) . (board!!)) mainDiagIndex
    || all ((==(head . show) player) . (board!!)) offDiagIndex
        where rows = chunksOf dim board
              consecutive = (==True) . all (\x -> x == (head . show) player)
              mainDiagIndex = [0, dim+1 .. size-1]
              offDiagIndex = [dim-1, dim*2-2 .. size-dim]
              --offDiagIndex = [2, 4, 6]

-- |Determine whether the game has ended.
isEnd :: GameState Board Turn -> Bool
isEnd game = isWinFor game X || isWinFor game O || tiedGame
    where tiedGame = null (possibleMoves game)
