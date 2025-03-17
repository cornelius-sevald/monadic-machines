{-# LANGUAGE LambdaCase #-}

module Automata.DFASpec where

import Automata.Class
import Automata.DFA
import Data.Alphabet
import qualified Data.Set as Set
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

spec :: Spec
spec = do
  describe "Example DFAs" $ do
    context "With L = {w | w contains an even number of 'O's}" $ do
      let (lang, langComp) = (evenOs, unevenOs)
      let dfa = dfaEvenOs
      prop "accepts strings in L" $ do
        (`shouldSatisfy` accepts dfa) <$> lang
      prop "rejects strings not in L" $ do
        (`shouldSatisfy` (not . accepts dfa)) <$> langComp
    context "With L = {w | w ends in 'I'}" $ do
      let (lang, langComp) = (endsInI, endsNotInI)
      let dfa = dfaEndsInI
      prop "accepts strings in L" $ do
        (`shouldSatisfy` accepts dfa) <$> lang
      prop "rejects strings not in L" $ do
        (`shouldSatisfy` (not . accepts dfa)) <$> langComp
    context "With L = {w | w contains no 'A's}" $ do
      let (lang, langComp) = (containsNoAs, containsAs)
      let dfa = dfaContainsNoAs
      prop "accepts strings in L" $ do
        (`shouldSatisfy` accepts dfa) <$> lang
      prop "rejects strings not in L" $ do
        (`shouldSatisfy` (not . accepts dfa)) <$> langComp

{- Example DFAs and associated languages -}

mkLangGen :: (Arbitrary a) => ([a] -> Bool) -> (Gen [a], Gen [a])
mkLangGen p = (arbitrary `suchThat` p, arbitrary `suchThat` (not . p))

evenOs, unevenOs :: Gen [Bit]
(evenOs, unevenOs) = mkLangGen p
  where
    p = even . length . filter (== O)

endsInI, endsNotInI :: Gen [Bit]
(endsInI, endsNotInI) = mkLangGen p
  where
    p w = not (null w) && last w == I

containsNoAs, containsAs :: Gen [ABC]
(containsNoAs, containsAs) = mkLangGen p
  where
    p = (A `notElem`)

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

dfaContainsNoAs :: DFA ABC Int
dfaContainsNoAs =
  DFA
    { start = 1,
      final = Set.singleton 1,
      trans = \case
        -- In state 1, we transition to the "dead" state 2 on an `A`.
        (1, A) -> 2
        (1, B) -> 1
        (1, C) -> 1
        -- In state 2 we are stuck.
        (2, A) -> 2
        (2, B) -> 2
        (2, C) -> 2
        _ -> undefined
    }
