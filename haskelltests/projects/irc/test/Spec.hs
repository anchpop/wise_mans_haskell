import Test.Hspec
import Test.QuickCheck
import Lib

main :: IO ()
main = hspec $ do
    describe "Lib.isValidChannelName" $ do
      it "disallows empty strings" $ do
        isValidChannelName "" `shouldBe` (False :: Bool)

      it "allows a channel with # in the first character" $ do
        isValidChannelName "#haskell" `shouldBe` (True :: Bool)
        isValidChannelName "#ghc" `shouldBe` (True :: Bool)

      it "disallows channels without # in the first character" $
        isValidChannelName "haskell" `shouldBe` (False :: Bool)
        
      it "allowes channels should contain a '#'" $ do
        property $ \xs -> if isValidChannelName xs
                           then elem '#' (xs :: String)
                           else True

      it "disallows a string with a space" $ do
        isValidChannelName "#ha skell" `shouldBe` (False :: Bool)

      it "disallows channel names with spaces" $ do
        property $ \xs -> isValidChannelName (xs ++ " ") == False