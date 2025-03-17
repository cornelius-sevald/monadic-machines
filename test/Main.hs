module Main (main) where

import qualified Automata.DFASpec
import qualified Automata.NFASpec
import qualified Data.ListListSpec
import qualified Data.PropositionSpec
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Data" $ do
    describe "ListList" Data.ListListSpec.spec
  describe "Data" $ do
    describe "Proposition" Data.PropositionSpec.spec
  describe "Automata" $ do
    describe "DFA" Automata.DFASpec.spec
    describe "NFA" Automata.NFASpec.spec
