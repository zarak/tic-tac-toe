import GameState
import Test.Hspec

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
                              ++"   ") `shouldBe` " X | X | 2 \n\
                                                  \-----------\n\
                                                  \ 3 | O | O \n\
                                                  \-----------\n\
                                                  \ 6 | 7 | 8 \n"
