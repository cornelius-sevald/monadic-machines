module Main (main) where

import qualified Automata.DFASpec
import qualified Data.ListListSpec
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Data" $ do
    describe "ListList" Data.ListListSpec.spec
  describe "Automata" $ do
    describe "DFA" Automata.DFASpec.spec
