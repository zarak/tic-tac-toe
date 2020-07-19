import GameState
import Test.Hspec

renderedBoard = " X | X | 2 \n\
                \-----------\n\
                \ 3 | O | O \n\
                \-----------\n\
                \ 6 | 7 | 8 \n"
initialBoard = "OO \
               \ X \
               \   "
finalBoard =   "OOX\
               \ X \
               \   "
firstRowXBoard =   "XXX\
                   \   \
                   \   "
secondRowXBoard =   "   \
                    \XXX\
                    \   "
thirdRowXBoard =   "   \
                   \   \
                   \XXX"
firstColOBoard =   "O  \
                   \O  \
                   \O  "
secondColOBoard =   " O \
                    \ O \
                    \ O "
thirdColOBoard =   "  O\
                   \  O\
                   \  O"


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
            it "should be a win for X on the first row" $ do
                isWinFor (GameState (Board firstRowXBoard) X) X `shouldBe` True
            it "should be a win for X on the second row" $ do
                isWinFor (GameState (Board secondRowXBoard) X) X `shouldBe` True
            it "should be a win for X on the third row" $ do
                isWinFor (GameState (Board thirdRowXBoard) X) X `shouldBe` True
        context "isEnd" $ do
            it "should determine it is the end for X" $ do
                isEnd (GameState (Board firstRowXBoard) X) `shouldBe` True
