module Main where

import Test.Hspec
import SectorSpec

main :: IO ()
main = hspec $ do
  describe "Sector" SectorSpec.spec
