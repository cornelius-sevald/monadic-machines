{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLists #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Automata.PushDown.Monadic.PropositionSpec where

import qualified Automata.PushDown.Monadic as MPDA
import Automata.PushDown.Monadic.Proposition (PropositionPDA)
import qualified Automata.PushDown.Monadic.Proposition as PropositionPDA
import Automata.PushDown.Util (Bottomed (..))
import Data.Alphabet
import Data.Logic.Proposition
import Data.NAry (NAry)
import qualified Data.NAry as NAry
import qualified Data.Set as Set
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.Util

spec :: Spec
spec = do
  describe "Example PropositionPDAs" $ do
    context "With L = { AⁿBⁿCⁿ | n ≥ 0 }" $ do
      let (lang, langComp) = (langEQ, langCompEQ)
      let pda = pdaEQ
      prop "accepts strings in L" $ do
        (`shouldSatisfy` PropositionPDA.accepts pda) <$> lang
      prop "rejects strings not in L" $ do
        (`shouldNotSatisfy` PropositionPDA.accepts pda) <$> langComp
    context "With L = { x#y | x ∈ EQ, y ∈ complement(EQ) }" $ do
      let (lang, langComp) = (langEQNonEQ, langCompEQNonEQ)
      let pda = pdaEQNonEQ
      prop "accepts strings in L" $ do
        (`shouldSatisfy` PropositionPDA.accepts pda) <$> lang
      prop "rejects strings not in L" $ do
        (`shouldNotSatisfy` PropositionPDA.accepts pda) <$> langComp
  describe "The 'invert' function" $ do
    context "With L = { AⁿBⁿCⁿ | n ≥ 0 }" $ do
      let (lang, langComp) = (langEQ, langCompEQ)
      let pda = PropositionPDA.invert pdaEQ
      prop "accepts strings not L" $ do
        (`shouldSatisfy` PropositionPDA.accepts pda) <$> langComp
      prop "rejects strings in L" $ do
        (`shouldNotSatisfy` PropositionPDA.accepts pda) <$> lang
    context "With L = { x#y | x ∈ EQ, y ∈ complement(EQ) }" $ do
      let (lang, langComp) = (langEQNonEQ, langCompEQNonEQ)
      let pda = PropositionPDA.invert pdaEQNonEQ
      prop "accepts strings not in L" $ do
        (`shouldSatisfy` PropositionPDA.accepts pda) <$> langComp
      prop "rejects strings in L" $ do
        (`shouldNotSatisfy` PropositionPDA.accepts pda) <$> lang

-- | Proposition PDA that recognizes
-- the EQ language { AⁿBⁿCⁿ | n ≥ 0 }.
--
-- The construction is based on the description
-- of a probabilistic PDA in [1, p. 987].
pdaEQ ::
  PropositionPDA
    (Either (Maybe (NAry 2)) (ABC, NAry 2))
    (Maybe ABC, NAry 2, NAry 3)
    ABC
    (Bottomed ())
pdaEQ =
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
         in pure (f 1) :/\: pure (f 2)
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

-- | Proposition PDA that recognizes
-- the language { x#y | x ∈ EQ, y ∈ comp(EQ) }.
--
-- This demonstrates that proposition (i.e. alternating) PDAs can recognize
-- languages that are neither in CFL nor co-CFL.
pdaEQNonEQ = pdaEQ `PropositionPDA.concatenateMarked` PropositionPDA.invert pdaEQ

{- Bibliography
 - ~~~~~~~~~~~~
 - [1]: Hromkovič, J., & Schnitger, G. (2010). On probabilistic pushdown automata.
 -        Information and Computation, 208(8), 982-995.
-}
