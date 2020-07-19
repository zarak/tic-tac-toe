module Lib where

data Three = 
        One 
      | Two 
      | Three 
      deriving (Show, Eq, Ord, Enum)

newtype TicTacToe a = TicTacToe2
    { board :: Three -> Three -> a 
    }

instance Show f => Show (TicTacToe f) where
    show (TicTacToe2 f) = show (f One One)

emptyBoard :: TicTacToe (Maybe Bool)
emptyBoard = TicTacToe2 $ const $ const Nothing


someFunc :: IO ()
someFunc = putStrLn "someFunc"
