{-# LANGUAGE DataKinds #-}

module Automata.NFASpec where

import qualified Automata.DFA as DFA
import Automata.NFA (NFA)
import qualified Automata.NFA as NFA
import Data.Alphabet
import Data.NAry (NAry)
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.Util

-- | The number of states of the NFAs we test.
type N = 8

-- | The type of states used in the tests.
type S = NAry N

-- | The alphabet we use for the tests.
type A = ABC

spec :: Spec
spec = do
  describe "toDFA" $ do
    prop "recognizes the same language" $ do
      \nfa' w ->
        let nfa = mkNFA nfa' :: NFA A S
            dfa = NFA.toDFA nfa
         in DFA.accepts dfa w `shouldBe` NFA.accepts nfa w
  describe "fromDFA" $ do
    prop "recognizes the same language" $ do
      \dfa' w ->
        let dfa = mkDFA dfa'
            nfa = NFA.fromDFA dfa :: NFA A S
         in NFA.accepts nfa w `shouldBe` DFA.accepts dfa w
