import GameState
import Test.Hspec

renderedBoard = " X | X | 2 \n\
                \-----------\n\
                \ 3 | O | O \n\
                \-----------\n\
                \ 6 | 7 | 8 \n"
initialBoard =       "OO \
                     \ X \
                     \   "
finalBoard =         "OOX\
                     \ X \
                     \   "
firstRowXBoard =     "XXX\
                     \   \
                     \   "
secondRowXBoard =    "   \
                     \XXX\
                     \   "
thirdRowXBoard =     "   \
                     \   \
                     \XXX"
firstColOBoard =     "O  \
                     \O  \
                     \O  "
secondColOBoard =    " O \
                     \ O \
                     \ O "
thirdColOBoard =     "  O\
                     \  O\
                     \  O"
mainDiagOBoard =     "O  \
                     \ O \
                     \  O"
offDiagOBoard =      "  O\
                     \ O \
                     \O  "
mainDiagXBoard =     "X  \
                     \ X \
                     \  X"
offDiagXBoard =      "  X\
                     \ X \
                     \X  "
tiedBoard =          "OXO\
                     \OXO\
                     \XOX"



main :: IO ()
main = hspec $ do
    describe "GameState" $ do
        context "initBoard" $ do
            it "should create an initial board" $ do
                initBoard `shouldBe` (GameState (Board $ "   " ++ "   " ++ "   ") X)
        context "render" $ do
            it "should render a position" $ do
                render (Board $ "XX "
                              ++" OO"
                              ++"   ") `shouldBe` renderedBoard
        context "move" $ do
            it "should make a move" $ do
                move (GameState (Board initialBoard) X) 2 `shouldBe` (GameState (Board finalBoard) O)
        context "possibleMoves" $ do
            it "should list the next available moves" $ do
                possibleMoves (GameState (Board initialBoard) X) `shouldBe` [2, 3, 5, 6, 7, 8]
        context "isStart" $ do
            it "should be true for the starting board" $ do
                isStart initBoard `shouldBe` True
            it "should be false for a nonempty board" $ do
                isStart (GameState (Board finalBoard) X) `shouldBe` False
        context "isWinFor" $ do
            it "should not be a win for X on an empty board" $ do
                isWinFor initBoard X `shouldBe` False
            it "should not be a win for O on an empty board" $ do
                isWinFor initBoard O `shouldBe` False
            it "should not confuse wins between players" $ do
                isWinFor (GameState (Board firstRowXBoard) X) O `shouldBe` False
            it "should be a win for X on the first row" $ do
                isWinFor (GameState (Board firstRowXBoard) X) X `shouldBe` True
            it "should be a win for X on the second row" $ do
                isWinFor (GameState (Board secondRowXBoard) X) X `shouldBe` True
            it "should be a win for X on the third row" $ do
                isWinFor (GameState (Board thirdRowXBoard) X) X `shouldBe` True
            it "should be a win for O on the first column" $ do
                isWinFor (GameState (Board firstColOBoard) X) O `shouldBe` True
            it "should be a win for O on the second column" $ do
                isWinFor (GameState (Board secondColOBoard) X) O `shouldBe` True
            it "should be a win for O on the third column" $ do
                isWinFor (GameState (Board thirdColOBoard) X) O `shouldBe` True

            it "should be a win for O on the main diagonal" $ do
                isWinFor (GameState (Board mainDiagOBoard) X) O `shouldBe` True
            it "should be a win for O on the off diagonal" $ do
                isWinFor (GameState (Board offDiagOBoard) X) O `shouldBe` True
            it "should be a win for X on the off diagonal" $ do
                isWinFor (GameState (Board offDiagXBoard) X) X `shouldBe` True
            it "should be a win for X on the main diagonal" $ do
                isWinFor (GameState (Board mainDiagXBoard) X) X `shouldBe` True
        context "isEnd" $ do
            it "should determine the game is over because X won" $ do
                isEnd (GameState (Board firstRowXBoard) O) `shouldBe` True
            it "should determine the game is over due to a tie" $ do
                isEnd (GameState (Board tiedBoard) X) `shouldBe` True
