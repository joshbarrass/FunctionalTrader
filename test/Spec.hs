module Main where

import Test.Hspec
import SectorSpec
import TradingSpec

main :: IO ()
main = hspec $ do
  describe "Sector" SectorSpec.spec
  describe "Trading" TradingSpec.spec
