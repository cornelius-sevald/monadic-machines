{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLists #-}

module Automata.FiniteState.Monadic.ProbabilitySpec where

import Automata.FiniteState.Monadic
import Automata.FiniteState.Monadic.Probability
import Data.Alphabet
import Data.Monoid (Product (..))
import Data.Monoid.HT (power)
import Data.Ratio
import qualified Numeric.Probability.Distribution as Dist
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

spec :: Spec
spec = do
  describe "Example ProbabilityFAs" $ do
    context "With L = { Iᵏ¹OIᵏ²O ... IᵏⁿO | ∏ⁿᵢ₌₁ (1 - (1/2)ᵏⁱ) > 1/2 }" $ do
      let η = 1 % 2
      let (lang, langComp) = langStochastic η
      let pfa = pfaStochastic
      prop "accepts strings in L" $ do
        (`shouldSatisfy` accepts pfa η) <$> lang
      prop "rejects strings not in L" $ do
        (`shouldSatisfy` (not . accepts pfa η)) <$> langComp

-- | The language { Iᵏ¹OIᵏ²O ... IᵏⁿO | ∏ⁿᵢ₌₁ (1 - (1/2)ᵏⁱ) > 1/2 }.
--
-- It is a non-regular, but stochastic language.
-- Taken from [1].
langStochastic :: Rational -> (Gen [Bit], Gen [Bit])
langStochastic η = (lang, langComp)
  where
    lang = suchThatMap langEither (either Just (const Nothing))
    langComp = suchThatMap langEither (either (const Nothing) Just)
    langEither = do
      -- Generate a list of random non-negative integers.
      ks' <-
        let f = fmap getNonNegative
         in f <$> arbitrary :: Gen [Integer]
      -- To make sure it doesn't blow up,
      -- we limit ourselves to 5 numbers of size at most 9.
      let ks = take 5 $ map (`mod` 10) ks'
      -- This function computes 1 - (1/2)ᵏ
      let φ k = getProduct $ 1 - power k (Product (1 % 2))
      -- We compute the word.
      let word = concatMap (\k -> power k [I] ++ [O]) ks
      -- If the product of φ over the ks is greater than η,
      -- then the word is in the language.
      -- Otherwise, it is not in the language.
      let inL = product (map φ ks) > η
      pure (if inL then Left word else Right word)

-- | A probability FA that recognizes the non-regular stochastic language
-- { Iᵏ¹OIᵏ²O ... IᵏⁿO | ∏ⁿᵢ₌₁ (1 - (1/2)ᵏⁱ) > 1/2 }.
--
-- Taken from [1].
pfaStochastic :: ProbabilityFA Rational Bit Int
pfaStochastic = MonadicFA {trans = _trans, start = _start, final = _final}
  where
    _start = 1
    _final = [1]
    _trans = \case
      (1, I) -> Dist.uniform [1, 2]
      (2, I) -> pure 2
      (2, O) -> pure 1
      _ -> pure 0

{- Bibliography
 - ~~~~~~~~~~~~
 - [1]: Florent Garnier, Stochastic languages, April 14, 2009
 -        URL: https://www-verimag.imag.fr/~iosif/LogicAutomata07/CoursStochLang.pdf
-}
