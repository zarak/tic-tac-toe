# tic-tac-toe

[![Build Status](https://travis-ci.org/zarak/tic-tac-toe.svg?branch=master)](https://travis-ci.org/zarak/tic-tac-toe)
![Continuous Integration](https://github.com/zarak/tic-tac-toe/workflows/Continuous%20Integration/badge.svg)

## Install

## Game Board
The game begins with a `3x3` grid with numbers indicating available moves.
Player `X` always goes first.
```
 0 | 1 | 2
-----------
 3 | 4 | 5
-----------
 6 | 7 | 8

Turn: X
Enter move>
```

## Move
Entering a digit marks the corresponding cell with the current player's symbol.
```
 0 | 1 | 2
-----------
 3 | X | 5
-----------
 6 | 7 | 8

Turn: O
Enter move>
3

 0 | 1 | 2
-----------
 O | X | 5
-----------
 6 | 7 | 8

Turn: X
Enter move>
```
