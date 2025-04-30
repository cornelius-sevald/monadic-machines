{-# LANGUAGE LambdaCase #-}

module Automata.FiniteState.DFASpec where

import Automata.FiniteState.DFA
import Data.Alphabet
import qualified Data.Set as Set
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.Util (langContainsAs, langCompContainsAs, langEndsInI, langCompEndsInI, langEvenOs, langCompEvenOs)

spec :: Spec
spec = do
  describe "Example DFAs" $ do
    context "With L = {w | w contains an even number of 'O's}" $ do
      let (lang, langComp) = (langEvenOs, langCompEvenOs)
      let dfa = dfaEvenOs
      prop "accepts strings in L" $ do
        (`shouldSatisfy` accepts dfa) <$> lang
      prop "rejects strings not in L" $ do
        (`shouldSatisfy` (not . accepts dfa)) <$> langComp
    context "With L = {w | w ends in 'I'}" $ do
      let (lang, langComp) = (langEndsInI, langCompEndsInI)
      let dfa = dfaEndsInI
      prop "accepts strings in L" $ do
        (`shouldSatisfy` accepts dfa) <$> lang
      prop "rejects strings not in L" $ do
        (`shouldSatisfy` (not . accepts dfa)) <$> langComp
    context "With L = {w | w contains 'A's}" $ do
      let (lang, langComp) = (langContainsAs, langCompContainsAs)
      let dfa = dfaContainsAs
      prop "accepts strings in L" $ do
        (`shouldSatisfy` accepts dfa) <$> lang
      prop "rejects strings not in L" $ do
        (`shouldSatisfy` (not . accepts dfa)) <$> langComp

{- Example DFAs and associated languages -}

dfaEvenOs :: DFA Bit Int
dfaEvenOs =
  DFA
    { start = 1,
      final = Set.singleton 1,
      trans = \case
        -- In state 1, we transition to state 2 on an `O`
        (1, O) -> 2
        (1, I) -> 1
        -- In state 2, we transition to back to state 1 on an `O`
        (2, O) -> 1
        (2, I) -> 2
        _ -> undefined
    }

dfaEndsInI :: DFA Bit Int
dfaEndsInI =
  DFA
    { start = 1,
      final = Set.singleton 2,
      trans = \case
        -- In either state, we transition to state 2
        -- (the only final state) on a `I`.
        (1, O) -> 1
        (1, I) -> 2
        (2, O) -> 1
        (2, I) -> 2
        _ -> undefined
    }

dfaContainsAs :: DFA ABC Int
dfaContainsAs =
  DFA
    { start = 1,
      final = Set.singleton 2,
      trans = \case
        -- In state 1, we transition to the final state 2 on an `A`.
        (1, A) -> 2
        (1, B) -> 1
        (1, C) -> 1
        -- In state 2 we are stuck.
        (2, A) -> 2
        (2, B) -> 2
        (2, C) -> 2
        _ -> undefined
    }
