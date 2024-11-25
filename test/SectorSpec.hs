module SectorSpec (
  spec
                  ) where

import Test.Hspec

import Sector
import Goods


spec :: Spec
spec = do
  describe "doesBuy" $ do
    context "when port does buy the good" $ do
      let good = getGoodByID 1
      let sec = Sector 1 Nothing Nothing Nothing Nothing Nothing 0 0 [good] [] [] []
      it "returns True" $ do
        (sec `doesBuy` good) `shouldBe` True
    context "when port does not buy the good" $ do
      let good = getGoodByID 1
      let otherGood = getGoodByID 2
      let sec = Sector 1 Nothing Nothing Nothing Nothing Nothing 0 0 [good] [] [] []
      it "returns False" $ do
        (sec `doesBuy` otherGood) `shouldBe` False
    context "when port doesn't buy anything" $ do
      let good = getGoodByID 1
      let sec = Sector 1 Nothing Nothing Nothing Nothing Nothing 0 0 [] [] [] []
      it "returns False" $ do
        (sec `doesBuy` good) `shouldBe` False

  describe "doesSell" $ do
    context "when port does sell the good" $ do
      let good = getGoodByID 1
      let sec = Sector 1 Nothing Nothing Nothing Nothing Nothing 0 0 [] [good] [] []
      it "returns True" $ do
        (sec `doesSell` good) `shouldBe` True
    context "when port does not sell the good" $ do
      let good = getGoodByID 1
      let otherGood = getGoodByID 2
      let sec = Sector 1 Nothing Nothing Nothing Nothing Nothing 0 0 [] [good] [] []
      it "returns False" $ do
        (sec `doesSell` otherGood) `shouldBe` False
    context "when port doesn't sell anything" $ do
      let good = getGoodByID 1
      let sec = Sector 1 Nothing Nothing Nothing Nothing Nothing 0 0 [] [] [] []
      it "returns False" $ do
        (sec `doesSell` good) `shouldBe` False

  describe "distanceTo" $ do
    context "when searching for the same sector" $ do
      let sec = Sector 1 Nothing Nothing Nothing Nothing Nothing 0 0 [] [] [] []
      it "returns Just 0" $ do
        distanceTo sec (== sec) `shouldBe` Just 0
    context "when searching for a sector that does not exist" $ do
      let sec = Sector 1 Nothing Nothing Nothing Nothing Nothing 0 0 [] [] [] []
      it "returns Nothing" $ do
        distanceTo sec (\x -> num x == 2) `shouldBe` Nothing
    context "when searching for an adjacent sector" $ do
      let secs = [Sector 1 Nothing Nothing Nothing (Just $ secs !! 1) Nothing 0 0 [] [] [] []
                 ,Sector 2 Nothing Nothing (Just $ secs !! 0) Nothing Nothing 0 0 [] [] [] []
                 ]
      let sec1 = secs !! 0
      let sec2 = secs !! 1
      it "returns Just 1" $ do
        distanceTo sec1 (== sec2) `shouldBe` Just 1
    context "when searching for a far away sector" $ do
      let distance = 20
      let secs = [Sector 0 (Just $ secs !! 1) (Just Wall) (Just Wall) (Just Wall) (Just Wall) 0 0 [] [] [] []]
                  ++ [Sector (toInteger i) (Just $ secs !! (i+1)) (Just $ secs !! (i-1)) (Just Wall) (Just Wall) (Just Wall) 0 0 [] [] [] []| i <- [1..(fromInteger distance - 1)] :: [Int]]
                  ++ [Sector distance (Just Wall) (Just $ secs !! (fromInteger distance - 1)) (Just Wall) (Just Wall) (Just Wall) 0 0 [] [] [] []]
      let sec1 = secs !! 0
      let sec2 = secs !! fromInteger distance
      it "returns the distance" $ do
        distanceTo sec1 (== sec2) `shouldBe` Just distance
        
    context "when searching via a warp" $ do
      context "when the warp is slower" $ do
        let distance = 3
        let secs = [Sector 0 (Just $ secs !! 1) (Just Wall) (Just Wall) (Just Wall) (Just $ secs !! fromInteger distance) 0 0 [] [] [] []]
                   ++ [Sector (toInteger i) (Just $ secs !! (i+1)) (Just $ secs !! (i-1)) (Just Wall) (Just Wall) (Just Wall) 0 0 [] [] [] [] | i <- [1..(fromInteger distance - 1)] :: [Int]]
                   ++ [Sector distance (Just Wall) (Just $ secs !! (fromInteger distance - 1)) (Just Wall) (Just Wall) (Just $ head secs) 0 0 [] [] [] []]
        let sec1 = secs !! 0
        let sec2 = secs !! fromInteger distance
        it "returns the non-warp distance" $ do
          distanceTo sec1 (== sec2) `shouldBe` Just distance

      context "when the warp is faster" $ do
        let distance = 20
        let secs = [Sector 0 (Just $ secs !! 1) (Just Wall) (Just Wall) (Just Wall) (Just $ secs !! fromInteger distance) 0 0 [] [] [] []]
                   ++ [Sector (toInteger i) (Just $ secs !! (i+1)) (Just $ secs !! (i-1)) (Just Wall) (Just Wall) (Just Wall) 0 0 [] [] [] []| i <- [1..(fromInteger distance - 1)] :: [Int]]
                   ++ [Sector distance (Just Wall) (Just $ secs !! (fromInteger distance - 1)) (Just Wall) (Just Wall) (Just $ head secs) 0 0 [] [] [] []]
        let sec1 = secs !! 0
        let sec2 = secs !! fromInteger distance
        it "returns the warp distance" $ do
          distanceTo sec1 (== sec2) `shouldBe` Just 5
