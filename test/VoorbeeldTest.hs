import Test.Hspec

import VoorbeeldModule (hoi, hallo)

main :: IO ()
main = hspec $ do 
    it "Returns correct string for hoi" $ do
        hoi `shouldBe` "Hoi"

    it "Returns correct string for hallo" $ do
        hallo `shouldBe` "Hallo"
