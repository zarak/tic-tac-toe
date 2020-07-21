# tic-tac-toe

![Continuous Integration](https://github.com/zarak/tic-tac-toe/workflows/Continuous%20Integration/badge.svg)  

![](https://media.istockphoto.com/photos/tic-tac-toe-game-picture-id884358528?k=6&m=884358528&s=612x612&w=0&h=0rihYQYvBX7sPnugkddA77c0uDswevGaRD9xScW5kM0=)

## Install
Requires GHC 8.8.3 and stack.
Issue the following commands to download the source code and install the
`tic-tac-toe-exe` binary.
```
git clone https://github.com/zarak/tic-tac-toe.git
stack install
tic-tac-toe-exe
```

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
