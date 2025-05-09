{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Automata.PushDown.Monadic.ProbabilitySpec where

import qualified Automata.PushDown.Monadic as MPDA
import qualified Automata.PushDown.Monadic.ListSpec as ListPDASpec
import Automata.PushDown.Monadic.Probability (ProbabilityPDA)
import qualified Automata.PushDown.Monadic.Probability as ProbabilityPDA
import Automata.PushDown.Util (Bottomed (..), end)
import Control.Arrow ((***))
import Data.Alphabet
import Data.NAry (NAry)
import qualified Data.NAry as NAry
import Data.Ratio
import qualified Data.Set as Set
import GHC.TypeLits
import qualified Numeric.Probability.Distribution as Dist
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.Util

spec :: Spec
spec = do
  describe "Example ProbabilityPDAs" $ do
    context "With L = { AⁿBⁿCⁿ | n ≥ 0 } (the EQ lang)" $ do
      let k = 3 :: NAry 3
      let η = 1 % fromIntegral k
      let (lang, langComp) = (langEQ, langCompEQ)
      let pda = pdaEQ k
      prop "accepts strings in L" $ do
        (`shouldSatisfy` ProbabilityPDA.accepts pda η) <$> lang
      prop "rejects strings not in L" $ do
        (`shouldNotSatisfy` ProbabilityPDA.accepts pda η) <$> langComp
    context "With L = { x#y | x ∈ EQ, y ∈ complement(EQ) }" $ do
      let k = 3 :: NAry 3
      let η = 1 % fromIntegral k
      let (lang, langComp) = (langEQNonEQ, langCompEQNonEQ)
      let pda = pdaEQNonEQ k
      prop "accepts strings in L" $ do
        (`shouldSatisfy` ProbabilityPDA.accepts pda η) <$> lang
      prop "rejects strings not in L" $ do
        (`shouldNotSatisfy` ProbabilityPDA.accepts pda η) <$> langComp
  describe "the 'invert' function" $ do
    context "With L = { AⁿBⁿCⁿ | n ≥ 0 } (the EQ lang)" $ do
      let k = 3 :: NAry 3
      let η = 1 % fromIntegral k
      let (lang, langComp) = (langEQ, langCompEQ)
      let pda = ProbabilityPDA.invert $ pdaEQ k
      prop "accepts strings not in L" $ do
        (`shouldSatisfy` ProbabilityPDA.accepts pda η) <$> langComp
      prop "rejects strings in L" $ do
        (`shouldNotSatisfy` ProbabilityPDA.accepts pda η) <$> lang
    context "With L = { x#y | x ∈ EQ, y ∈ complement(EQ) }" $ do
      let k = 3 :: NAry 3
      let η = 1 % fromIntegral k
      let (lang, langComp) = (langEQNonEQ, langCompEQNonEQ)
      let pda = ProbabilityPDA.invert $ pdaEQNonEQ k
      prop "accepts strings not L" $ do
        (`shouldSatisfy` ProbabilityPDA.accepts pda η) <$> langComp
      prop "rejects strings in L" $ do
        (`shouldNotSatisfy` ProbabilityPDA.accepts pda η) <$> lang
  describe "the 'fromAngelicListPDA' function" $
    modifyMaxSize (`div` 3) $ do
      let η = 2 % (3 :: Integer)
      let endP = fmap end *** fmap end
      context "With L = {OᵏIᵏ | k ≥ 0}" $ do
        let (lang, langComp) = endP (langOkIk, langCompOkIk)
        let pda = ProbabilityPDA.fromAngelicListPDA η ListPDASpec.pdaOkIk
        prop "accepts strings in L" $ do
          (`shouldSatisfy` ProbabilityPDA.accepts pda η) <$> lang
        prop "rejects strings not in L" $ do
          (`shouldNotSatisfy` ProbabilityPDA.accepts pda η) <$> langComp
      context "With L = {w | w is a palindrome}" $ do
        let (lang, langComp) = endP (langPalindromes, langCompPalindromes)
        let pda = ProbabilityPDA.fromAngelicListPDA η ListPDASpec.pdaPalindromes
        prop "accepts strings in L" $ do
          (`shouldSatisfy` ProbabilityPDA.accepts pda η) <$> lang
        prop "rejects strings not in L" $ do
          (`shouldNotSatisfy` ProbabilityPDA.accepts pda η) <$> langComp
      context "With L = {w·w | w ∈ Σ*}" $ do
        let (lang, langComp) = endP (langRepeated, langCompRepeated)
        let pda = ProbabilityPDA.fromAngelicListPDA η ListPDASpec.pdaNonRepeated
        prop "accepts strings not in L" $ do
          (`shouldSatisfy` ProbabilityPDA.accepts pda η) <$> langComp
        prop "rejects strings in L" $ do
          (`shouldNotSatisfy` ProbabilityPDA.accepts pda η) <$> lang

-- | Probabilistic PDA that recognizes
-- the EQ language { AⁿBⁿCⁿ | n ≥ 0 }
-- with false positive rate bounded by 1/k.
--
-- The construction is based on the description in [1, p. 987].
pdaEQ ::
  (KnownNat n, KnownNat (n + 1)) =>
  NAry n ->
  ProbabilityPDA
    Rational
    (Either (Maybe (NAry 2)) (ABC, NAry n))
    (Maybe ABC, NAry n, NAry (n + 1))
    ABC
    (Bottomed ())
pdaEQ k =
  MPDA.MonadicPDA
    { MPDA.startSymbol = _startSymbol,
      MPDA.startState = _startState,
      MPDA.finalStates = _finalStates,
      MPDA.transRead = _transRead,
      MPDA.transPop = _transPop
    }
  where
    _startSymbol = Bottom
    _startState = (Left . Just) 1
    _finalStates = Set.map (Left . Just) [1, 2]
    _transRead = \case
      (Left (Just 1), A) ->
        let f x = Right (Nothing, x, undefined)
         in f <$> Dist.uniform [1 .. k]
      (Right (A, x), A) ->
        pure $ Right (Just A, x, undefined)
      (Right (A, x), B) ->
        pure $ Right (Just B, x, undefined)
      (Right (B, x), B) ->
        pure $ Right (Just B, x, undefined)
      (Right (B, x), C) ->
        pure $ Right (Just C, x, NAry.safeSucc x)
      (Right (C, x), C) ->
        pure $ Right (Just C, x, NAry.safeSucc x)
      _ -> pure $ Left (Left Nothing, [])
    _transPop = \case
      ((Nothing, x, _), Bottom) ->
        pure $ Left (Right (A, x), [Bottom])
      ((Just A, x, _), t) ->
        pure $ Left (Right (A, x), [SSymbol (), t])
      ((Just B, x, _), t) ->
        pure $ Left (Right (B, x), replicate (fromIntegral x) (SSymbol ()) <> [t])
      ((Just C, x, y), SSymbol ()) ->
        case NAry.safePred' y of
          Nothing -> pure $ Left (Right (C, x), [])
          Just y' -> pure $ Right (Just C, x, y')
      ((Just C, _, y), Bottom)
        | y == 1 -> pure $ Left (Left $ Just 2, [Bottom])
        | otherwise -> pure $ Left (Left Nothing, [Bottom])
      c -> error $ "invalid pop state / stack input " ++ show c

-- | Probabilistic PDA that recognizes
-- the language { x#y | x ∈ EQ, y ∈ comp(EQ) }
-- with false positive chance bounded by (1/k).
--
-- This demonstrates that probabilistic PDAs can recognize
-- languages that are neither in CFL nor co-CFL.
pdaEQNonEQ k = pdaEQ k `ProbabilityPDA.concatenateMarked` ProbabilityPDA.invert (pdaEQ k)

{- Bibliography
 - ~~~~~~~~~~~~
 - [1]: Hromkovič, J., & Schnitger, G. (2010). On probabilistic pushdown automata.
 -        Information and Computation, 208(8), 982-995.
-}
