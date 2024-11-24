module SectorSpec (
  spec
                  ) where

import Test.Hspec

import Sector
import Goods


spec :: Spec
spec = do
  describe "doesBuy" $ do
    it "true" $ do
      let good = getGoodByID 1
      let sec = Sector 1 Nothing Nothing Nothing Nothing Nothing 0 0 [good] []
      (sec `doesBuy` good) `shouldBe` True
    it "false" $ do
      let good = getGoodByID 1
      let otherGood = getGoodByID 2
      let sec = Sector 1 Nothing Nothing Nothing Nothing Nothing 0 0 [good] []
      (sec `doesBuy` otherGood) `shouldBe` False
    it "null" $ do
      let good = getGoodByID 1
      let sec = Sector 1 Nothing Nothing Nothing Nothing Nothing 0 0 [] []
      (sec `doesBuy` good) `shouldBe` False

  describe "doesSell" $ do
    it "true" $ do
      let good = getGoodByID 1
      let sec = Sector 1 Nothing Nothing Nothing Nothing Nothing 0 0 [] [good]
      (sec `doesSell` good) `shouldBe` True
    it "false" $ do
      let good = getGoodByID 1
      let otherGood = getGoodByID 2
      let sec = Sector 1 Nothing Nothing Nothing Nothing Nothing 0 0 [] [good]
      (sec `doesSell` otherGood) `shouldBe` False
    it "null" $ do
      let good = getGoodByID 1
      let sec = Sector 1 Nothing Nothing Nothing Nothing Nothing 0 0 [] []
      (sec `doesSell` good) `shouldBe` False

  describe "distanceTo" $ do
    it "same sector" $ do
      let sec = Sector 1 Nothing Nothing Nothing Nothing Nothing 0 0 [] []
      distanceTo sec (== sec) `shouldBe` Just 0
    it "does not exist" $ do
      let sec = Sector 1 Nothing Nothing Nothing Nothing Nothing 0 0 [] []
      distanceTo sec (\x -> num x == 2) `shouldBe` Nothing
    it "adjacent" $ do
      let secs = [Sector 1 Nothing Nothing Nothing (Just $ secs !! 1) Nothing 0 0 [] []
                 ,Sector 2 Nothing Nothing (Just $ secs !! 0) Nothing Nothing 0 0 [] []
                 ]
      let sec1 = secs !! 0
      let sec2 = secs !! 1
      distanceTo sec1 (== sec2) `shouldBe` Just 1
    it "far" $ do
      let distance = 20
      let secs = [Sector 0 (Just $ secs !! 1) (Just Wall) (Just Wall) (Just Wall) (Just Wall) 0 0 [] []]
                  ++ [Sector (toInteger i) (Just $ secs !! (i+1)) (Just $ secs !! (i-1)) (Just Wall) (Just Wall) (Just Wall) 0 0 [] [] | i <- [1..(fromInteger distance - 1)] :: [Int]]
                  ++ [Sector distance (Just Wall) (Just $ secs !! (fromInteger distance - 1)) (Just Wall) (Just Wall) (Just Wall) 0 0 [] []]
      let sec1 = secs !! 0
      let sec2 = secs !! fromInteger distance
      distanceTo sec1 (== sec2) `shouldBe` Just distance
