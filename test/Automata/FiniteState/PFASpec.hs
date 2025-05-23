{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLists #-}

module Automata.FiniteState.PFASpec where

import Automata.FiniteState.PFA
import Data.Alphabet
import Data.NAry
import Data.Ratio
import qualified Numeric.Probability.Distribution as Dist
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.Util

spec :: Spec
spec = do
  describe "Example PFAs" $ do
    context "With L = { Iᵏ¹OIᵏ²O ... IᵏⁿO | ∏ⁿᵢ₌₁ (1 - (1/2)ᵏⁱ) > 2/3 }" $ modifyMaxSize id $ do
      let η = 2 % 3
      let (lang, langComp) = langStochastic η
      let pfa = pfaStochastic
      prop "accepts strings in L" $ do
        (`shouldSatisfy` accepts pfa η) <$> lang
      prop "rejects strings not in L" $ do
        (`shouldSatisfy` (not . accepts pfa η)) <$> langComp

{- Example PFAs and associated languages -}

-- | A probabilistic FA that recognizes the non-regular stochastic language
-- { Iᵏ¹OIᵏ²O ... IᵏⁿO | ∏ⁿᵢ₌₁ (1 - (1/2)ᵏⁱ) > η }.
--
-- Taken from [1].
pfaStochastic :: PFA Rational Bit (NAry 3)
pfaStochastic = PFA {trans = _trans, start = _start, final = _final}
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
