{-# LANGUAGE OverloadedStrings #-}
module Main where

import GameState

testBoard = Board "XXO X O  "

main :: IO ()
main = putStr $ "Test Board\n" ++ show testBoard

