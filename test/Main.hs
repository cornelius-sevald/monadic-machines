module Main (main) where

import qualified Automata.FiniteState.AFASpec
import qualified Automata.FiniteState.DFASpec
import qualified Automata.FiniteState.Monadic.IdentitySpec
import qualified Automata.FiniteState.Monadic.ListListSpec
import qualified Automata.FiniteState.Monadic.ListSpec
import qualified Automata.FiniteState.Monadic.ProbabilitySpec
import qualified Automata.FiniteState.Monadic.PropositionSpec
import qualified Automata.FiniteState.NFASpec
import qualified Automata.FiniteState.PFASpec
import qualified Automata.PushDown.DPDASpec
import qualified Automata.PushDown.FPDASpec
import qualified Automata.PushDown.Monadic.IdentitySpec
import qualified Automata.PushDown.Monadic.ListSpec
import qualified Automata.PushDown.Monadic.ProbabilitySpec
import qualified Automata.PushDown.Monadic.PropositionSpec
import qualified Automata.PushDown.NPDASpec
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
    describe "Finite-state Automata" $ do
      describe "DFA" Automata.FiniteState.DFASpec.spec
      describe "NFA" Automata.FiniteState.NFASpec.spec
      describe "AFA" Automata.FiniteState.AFASpec.spec
      describe "PFA" Automata.FiniteState.PFASpec.spec
      describe "Monadic FA" $ do
        describe "Identity FA" Automata.FiniteState.Monadic.IdentitySpec.spec
        describe "List FA" Automata.FiniteState.Monadic.ListSpec.spec
        describe "ListList FA" Automata.FiniteState.Monadic.ListListSpec.spec
        describe "Proposition FA" Automata.FiniteState.Monadic.PropositionSpec.spec
        describe "Probability FA" Automata.FiniteState.Monadic.ProbabilitySpec.spec
    describe "Pushdown Automata" $ do
      describe "Deterministic PDAs" $ do
        Automata.PushDown.DPDASpec.spec
      describe "Non-deterministic PDAs" $ do
        Automata.PushDown.NPDASpec.spec
      describe "Functional Deterministic PDAs" $ do
        Automata.PushDown.FPDASpec.spec
      describe "Monadic PDA" $ do
        describe "Identity PDA" Automata.PushDown.Monadic.IdentitySpec.spec
        describe "List PDA" Automata.PushDown.Monadic.ListSpec.spec
        describe "Proposition PDA" Automata.PushDown.Monadic.PropositionSpec.spec
        describe "Probability PDA" Automata.PushDown.Monadic.ProbabilitySpec.spec
