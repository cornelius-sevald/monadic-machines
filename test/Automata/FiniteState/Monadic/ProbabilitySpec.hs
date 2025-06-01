{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLists #-}

module Automata.FiniteState.Monadic.ProbabilitySpec where

import Automata.FiniteState.Monadic
import Automata.FiniteState.Monadic.Probability
import qualified Automata.FiniteState.PFA as PFA
import Data.Alphabet
import Data.NAry
import Data.Ratio
import qualified Numeric.Probability.Distribution as Dist
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.Util

-- | The number of states of the NFAs we test.
type N = 3

-- | The type of states used in the tests.
type S = NAry N

-- | The alphabet we use for the tests.
type A = ABC

spec :: Spec
spec = do
  describe "Example ProbabilityFAs" $ do
    context "With L = { Iᵏ¹OIᵏ²O ... IᵏⁿO | ∏ⁿᵢ₌₁ (1 - (1/2)ᵏⁱ) > 2/3 }" $ do
      let η = 2 % 3
      let (lang, langComp) = langStochastic η
      let pfa = pfaStochastic
      let acceptance m = acceptsExclusive m η
      prop "accepts(>) strings in L" $ do
        (`shouldSatisfy` acceptance pfa) <$> lang
      prop "rejects(>) strings not in L" $ do
        (`shouldNotSatisfy` acceptance pfa) <$> langComp
  describe "the 'invert' function" $ do
    context "With L = { Iᵏ¹OIᵏ²O ... IᵏⁿO | ∏ⁿᵢ₌₁ (1 - (1/2)ᵏⁱ) > 2/3 }" $ do
      let η = 2 % 3
      let (lang, langComp) = langStochastic η
      let pfa = invert pfaStochastic
      let acceptance m = acceptsInclusive m (1 - η)
      prop "accepts strings not in L" $ do
        (`shouldSatisfy` acceptance pfa) <$> langComp
      prop "rejects strings in L" $ do
        (`shouldNotSatisfy` acceptance pfa) <$> lang
  describe "toPFA" $ modifyMaxSize (`div` 10) $ do
    prop "recognizes(>) the same language" $ do
      \m' w ->
        let η = 2 % 3 -- arbitrary cut-point
            m = mkProbabilityFA m' :: ProbabilityFA Rational A S
            pfa = toPFA m
         in PFA.accepts pfa η w `shouldBe` acceptsExclusive m η w
  describe "fromPFA" $ modifyMaxSize (`div` 10) $ do
    prop "recognizes(>) the same language" $ do
      \pfa' w ->
        let η = 2 % 3 -- arbitrary cut-point
            pfa = mkPFA pfa'
            m = fromPFA pfa :: ProbabilityFA Rational A S
         in acceptsExclusive m η w `shouldBe` PFA.accepts pfa η w

-- | A probability FA that recognizes the non-regular stochastic language
-- { Iᵏ¹OIᵏ²O ... IᵏⁿO | ∏ⁿᵢ₌₁ (1 - (1/2)ᵏⁱ) > η }.
--
-- Taken from [1].
pfaStochastic :: ProbabilityFA Rational Bit (NAry 3)
pfaStochastic = MonadicFA {trans = _trans, start = _start, final = _final}
  where
    _start = 1
    _final = [1]
    _trans = \case
      (1, I) -> Dist.uniform [1, 2]
      (2, I) -> pure 2
      (2, O) -> pure 1
      _ -> pure 3

{- Bibliography
 - ~~~~~~~~~~~~
 - [1]: Florent Garnier, Stochastic languages, April 14, 2009
 -        URL: https://www-verimag.imag.fr/~iosif/LogicAutomata07/CoursStochLang.pdf
-}
