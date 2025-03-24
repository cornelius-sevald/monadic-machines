{-# LANGUAGE DataKinds #-}

module Automata.Monadic.ListSpec where

import Automata.Monadic.List
import qualified Automata.NFA as NFA
import Data.Alphabet
import Data.NAry (NAry)
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.Util

-- | The number of states of the FAs we test.
type N = 4

-- | The type of states used in the tests.
type S = NAry N

-- | The alphabet we use for the tests.
type A = ABC

spec :: Spec
spec = do
  describe "toNFA" $ modifyMaxSize isqrt $ do
    prop "recognizes the same language" $ do
      \m' w ->
        let m = mkMFA m' :: ListAutomaton A S
            nfa = toNFA m
         in NFA.accepts nfa w `shouldBe` accepts m w
  describe "fromNFA" $ modifyMaxSize isqrt $ do
    prop "recognizes the same language" $ do
      \nfa' w ->
        let nfa = mkNFA nfa'
            m = fromNFA nfa :: ListAutomaton A S
         in accepts m w `shouldBe` NFA.accepts nfa w
