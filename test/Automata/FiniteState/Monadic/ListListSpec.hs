{-# LANGUAGE DataKinds #-}

module Automata.FiniteState.Monadic.ListListSpec where

import qualified Automata.FiniteState.AFA as AFA
import Automata.FiniteState.Monadic.ListList
import Data.Alphabet
import Data.NAry (NAry)
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.Util

-- | The type of states used in the tests.
type S = NAry 4

-- | Extra small number of states, to avoid insane exponential blowup.
type S' = NAry 2

-- | The alphabet we use for the tests.
type A = ABC

spec :: Spec
spec = do
  describe "toAFA" $
    modifyMaxSize (const 5) $ do
      prop "recognizes the same language" $ do
        \m' w ->
          let m = mkMFA m' :: ListListFA A S
              afa = toAFA m
           in AFA.accepts afa w `shouldBe` acceptsDNF m w
  describe "fromAFA" $ do
    modifyMaxSize (const 5) $ do
      prop "recognizes the same language" $ do
        \afa' w ->
          -- We use an extra small number of states here.
          -- It does work for a larger number of states,
          -- but it takes too long to do every time.
          let afa = mkAFA afa' :: AFA.AFA A S'
              m = fromAFA afa
           in acceptsDNF m w `shouldBe` AFA.accepts afa w
