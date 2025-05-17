{-# LANGUAGE DataKinds #-}

module Automata.FiniteState.Monadic.ListSpec where

import Automata.FiniteState.Monadic.List
import qualified Automata.FiniteState.NFA as NFA
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
        let m = mkMFA m' :: ListFA A S
            nfa = toNFA m
         in NFA.accepts nfa w `shouldBe` acceptsAngelic m w
  describe "fromNFA" $ modifyMaxSize isqrt $ do
    prop "recognizes the same language" $ do
      \nfa' w ->
        let nfa = mkNFA nfa'
            m = fromNFA nfa :: ListFA A (Maybe S)
         in acceptsAngelic m w `shouldBe` NFA.accepts nfa w
