{-# LANGUAGE DataKinds #-}

module Automata.Monadic.PropositionSpec where

import qualified Automata.AFA as AFA
import Automata.Monadic.Proposition
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
  describe "toAFA" $ modifyMaxSize (`div` 10) $ do
    prop "recognizes the same language" $ do
      \m' w ->
        let m = mkMFA m' :: PropositionAutomaton A S
            afa = toAFA m
         in AFA.accepts afa w `shouldBe` accepts m w
  describe "fromAFA" $ do
    prop "recognizes the same language" $ do
      \afa' w ->
        let afa = mkAFA afa'
            m = fromAFA afa :: PropositionAutomaton A S
         in accepts m w `shouldBe` AFA.accepts afa w
