{-# LANGUAGE DataKinds #-}

module Automata.NFASpec where

import Automata.Class
import Automata.NFA
import Data.Alphabet
import Data.NAry (NAry)
import Test.Hspec
import Test.Hspec.QuickCheck

-- | The number of states of the NFAs we test.
type N = 8

-- | The type of states used in the tests.
type S = NAry N

-- | The alphabet we use for the tests.
type A = ABC

spec :: Spec
spec = do
  describe "toNFA" $ do
    prop "recognizes the same language" $ do
      \nfa w ->
        let dfa = toDFA (typNFA nfa)
         in accepts nfa w `shouldBe` accepts dfa w
  describe "fromDFA" $ do
    prop "recognizes the same language" $ do
      \dfa w ->
        let nfa = typNFA (fromDFA dfa)
         in accepts nfa w `shouldBe` accepts dfa w

-- | Like `id` but forces the desired type of the NFA.
typNFA :: NFA A S -> NFA A S
typNFA = id
