module GameState where

import Text.Paint
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
    show = colorize

data GameState board turn
    = GameState board turn 
    deriving Eq

instance (Show board, Show turn) => Show (GameState board turn) where
    show (GameState board turn) = "\n" ++ show board ++ "\nTurn: " ++ show turn

dim = 3
size = dim^2

red = Paint Maroon Default []
blue = Paint Blue Default []

initBoard :: GameState Board Turn
initBoard = GameState (Board $ replicate size ' ') X 

colorize :: Board -> String
colorize board = concatMap applyColor (render board)
    where applyColor c
            | c == 'X' = paint red [c]
            | c == 'O' = paint blue [c]
            | otherwise = [c]

render :: Board -> String
render (Board board) =
    unlines .
    intersperse (replicate 11 '-') .
    map (intercalate "|") .
    chunksOf dim .
    map (\(i, c) -> " " ++ (if c == ' ' then show i else [c]) ++ " ") $ 
    zip [0..] board

switchTurn :: Turn -> Turn
switchTurn X = O
switchTurn O = X

move :: GameState Board Turn -> Int -> GameState Board Turn
move (GameState (Board board) turn) idx =
    let newBoard = zipWith (\i c -> if i == idx then (head . show) turn else c) [0..] board
    in
    GameState (Board newBoard) (switchTurn turn)

possibleMoves :: GameState Board Turn -> [Int]
possibleMoves (GameState (Board board) turn) =
    map fst . filter ((==' ') . snd) $ zip [0..] board

isStart :: GameState Board Turn -> Bool
isStart (GameState (Board board) _) = all (==' ') board

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

isEnd :: GameState Board Turn -> Bool
isEnd game = isWinFor game X || isWinFor game O || tiedGame
    where tiedGame = null (possibleMoves game)
