module Lib where

data Three = 
        One 
      | Two 
      | Three 
      deriving (Show, Eq, Ord, Enum)

newtype TicTacToe a = TicTacToe2
    { board :: Three -> Three -> a 
    }

instance Show (TicTacToe a) where
    show (TicTacToe2 a) = "test"

emptyBoard :: TicTacToe (Maybe Bool)
emptyBoard = TicTacToe2 $ const $ const Nothing


someFunc :: IO ()
someFunc = putStrLn "someFunc"
