{-# LANGUAGE DataKinds #-}

module Automata.FiniteState.AFASpec where

import Data.Alphabet
import Data.NAry (NAry)
import Automata.FiniteState.AFA (AFA)
import qualified Automata.FiniteState.AFA as AFA
import qualified Automata.FiniteState.DFA as DFA
import qualified Automata.FiniteState.NFA as NFA
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.Util

-- | The number of states of the AFAs we test.
type N = 4

-- | The type of states used in the tests.
type S = NAry N

-- | The alphabet we use for the tests.
type A = ABC

spec :: Spec
spec = do
  --  NOTE: Currently fails, need to fix implementation
  describe "toNFA" $ modifyMaxSize (`div` 10) $ do
    prop "recognizes the same language" $ do
      \afa' w ->
        let afa = mkAFA afa' :: AFA A S
            nfa = AFA.toNFA afa
         in NFA.accepts nfa w `shouldBe` AFA.accepts afa w
  describe "fromDFA" $ modifyMaxSize (`div` 10) $ do
    prop "recognizes the same language" $ do
      \dfa' w ->
        let dfa = mkDFA dfa'
            afa = AFA.fromDFA dfa :: AFA A S
         in AFA.accepts afa w `shouldBe` DFA.accepts dfa w
  describe "fromNFA" $ modifyMaxSize (`div` 10) $ do
    prop "recognizes the same language" $ do
      \nfa' w ->
        let nfa = mkNFA nfa'
            afa = AFA.fromNFA nfa :: AFA A S
         in AFA.accepts afa w `shouldBe` NFA.accepts nfa w
