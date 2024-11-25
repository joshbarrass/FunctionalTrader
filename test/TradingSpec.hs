module TradingSpec (
  spec
                   ) where

import Test.Hspec

import Trading 
import Sector
import Goods

spec :: Spec
spec = do
  describe "fineChance" $ do
    context "when trade good is legal" $ do
      it "is 0" $ do
        let good = Good 1 "Test" 123 False
        let sector = Sector 1 Nothing Nothing Nothing Nothing Nothing 0 0 [good] [] [] []
        (fineChance good sector :: Double) `shouldBe` 0.0

    context "when trade good is illegal" $ do
      let illegalGoods = [
              Good 1 "Illegal 1" 123 True
            , Good 2 "Illegal 2" 123 True
            , Good 3 "Illegal 3" 123 True
            ]
      let good = head illegalGoods
      context "when port trades 1 illegal good" $ do
        let sector = Sector 1 Nothing Nothing Nothing Nothing Nothing 0 0 (take 1 illegalGoods) [] [] []
        it "is base chance − 4%" $ do
          (fineChance good sector :: Double) `shouldBe` (0.15 - 1*0.04)
      context "when port trades 2 illegal good" $ do
        let sector = Sector 1 Nothing Nothing Nothing Nothing Nothing 0 0 (take 2 illegalGoods) [] [] []
        it "is base chance − 8%" $ do
          (fineChance good sector :: Double) `shouldBe` (0.15 - 2*0.04)
      context "when port trades 3 illegal good" $ do
        let sector = Sector 1 Nothing Nothing Nothing Nothing Nothing 0 0 (take 3 illegalGoods) [] [] []
        it "is base chance − 12%" $ do
          (fineChance good sector :: Double) `shouldBe` (0.15 - 3*0.04)

  describe "getRelFactor" $ do
    context "when relationship is greater than 1000" $ do
      let range = [1000..2000]
      it "returns 1" $ do
        map getRelFactor range `shouldBe` [1.0 | _ <- range]
    context "when relationship is less than 1000" $ do
      let range = [0..1000]
      it "returns relationship / 1000" $ do
        map getRelFactor range `shouldBe` [fromIntegral i/1000 | i <- range]
