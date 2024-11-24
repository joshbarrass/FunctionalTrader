module SectorSpec (
  spec
                  ) where

import Test.Hspec

spec :: Spec
spec = do
  describe "test" $ do
    it "read" $ do
      read "10" `shouldBe` (10 :: Int)
