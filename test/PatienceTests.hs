import Test.Hspec
import DataStructures
import Logica

main :: IO()
main = hspec $ do
    it "Return the correct simple value of a cardValue" $ do
        simpleValue Three `shouldBe` "3"

    it "Test: correct result of flipCard" $ do
        flipCard (Spade, A, Hidden) `shouldBe` (Spade, A, Visible)
        flipCard (Spade, A, Visible) `shouldBe` (Spade, A, Visible)

    it "Test: correct result of isOneHigher" $ do
        isOneHigher (Spade, A, Visible) [] `shouldBe` True
        isOneHigher (Spade, Two, Visible) [(Spade, A, Visible)] `shouldBe` True
        isOneHigher (Spade, Two, Visible) [(Spade, A, Visible), (Spade, Two, Visible)] `shouldBe` False

    it "Test: correct result of rmLastCard" $ do
        rmLastCard [(Spade, A, Hidden), (Spade, Two, Visible)] `shouldBe` [(Spade, A, Visible)]

    it "Test: correct result of flipLastCard" $ do
        flipLastCard [] `shouldBe` []
        flipLastCard [(Spade, A, Hidden), (Spade, Two, Visible)] `shouldBe` [(Spade, A, Hidden), (Spade, Two, Visible)]
        flipLastCard [(Spade, A, Hidden), (Spade, Two, Hidden)] `shouldBe` [(Spade, A, Hidden), (Spade, Two, Visible)]