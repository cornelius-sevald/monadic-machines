{-# LANGUAGE DataKinds #-}

module Automata.Monadic.IdentitySpec where

import qualified Automata.DFA as DFA
import Automata.Monadic.Identity
import Data.Alphabet
import Data.NAry (NAry)
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.Util

-- | The number of states of the FAs we test.
type N = 8

-- | The type of states used in the tests.
type S = NAry N

-- | The alphabet we use for the tests.
type A = ABC

spec :: Spec
spec = do
  describe "toDFA" $ do
    prop "recognizes the same language" $ do
      \m' w ->
        let m = mkMFA m' :: IdentityAutomaton A S
            dfa = toDFA m
         in DFA.accepts dfa w `shouldBe` accepts m w
  describe "fromDFA" $ do
    prop "recognizes the same language" $ do
      \dfa' w ->
        let dfa = mkDFA dfa'
            m = fromDFA dfa :: IdentityAutomaton A S
         in accepts m w `shouldBe` DFA.accepts dfa w
