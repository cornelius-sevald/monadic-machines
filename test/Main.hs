module Main (main) where

import qualified Automata.AFASpec
import qualified Automata.DFASpec
import qualified Automata.Monadic.IdentitySpec
import qualified Automata.Monadic.ListSpec
import qualified Automata.Monadic.PropositionSpec
import qualified Automata.NFASpec
import qualified Data.ListListSpec
import qualified Data.Logic.NormalFormSpec
import qualified Data.Logic.PropositionSpec
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Data" $ do
    describe "ListList" Data.ListListSpec.spec
    describe "Logic" $ do
      describe "Proposition" Data.Logic.PropositionSpec.spec
      describe "NormalForm" Data.Logic.NormalFormSpec.spec
  describe "Automata" $ do
    describe "DFA" Automata.DFASpec.spec
    describe "NFA" Automata.NFASpec.spec
    describe "AFA" Automata.AFASpec.spec
    describe "Monadic Automata" $ do
      describe "Identity Automata" Automata.Monadic.IdentitySpec.spec
      describe "List Automata" Automata.Monadic.ListSpec.spec
      describe "Proposition Automata" Automata.Monadic.PropositionSpec.spec
