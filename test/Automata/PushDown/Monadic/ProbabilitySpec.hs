{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLists #-}

module Automata.PushDown.Monadic.ProbabilitySpec where

import qualified Automata.PushDown.Monadic as MPDA
import Automata.PushDown.Monadic.Probability (ProbabilityPDA)
import qualified Automata.PushDown.Monadic.Probability as ProbabilityPDA
import Automata.PushDown.Util (Bottomed (..))
import Control.Applicative (Alternative (empty))
import Data.Alphabet
import Data.List (genericReplicate)
import Data.Ratio
import qualified Numeric.Probability.Distribution as Dist
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.Util

spec :: Spec
spec = do
  describe "Example ProbabilityPDAs" $ do
    context "With L = { AⁿBⁿCⁿ | n ≥ 0 }" $ do
      let k = 3
      let η = 1 % k
      let (lang, langComp) = (langEQ, langCompEQ)
      let pda = pdaEQ k
      prop "accepts strings in L" $ do
        (`shouldSatisfy` ProbabilityPDA.accepts pda η) <$> lang
      prop "rejects strings not in L" $ do
        (`shouldNotSatisfy` ProbabilityPDA.accepts pda η) <$> langComp

-- | Probabilistic two-sided error PDA that recognizes
-- the EQ language { AⁿBⁿCⁿ | n ≥ 0 } with error bounded by 1/k.
--
-- The construction is based on the description in [1, p. 987].
--
-- It might seem that it breaks one of the requirements of a automaton,
-- namely that the number of states is finite (due to the non-finite 'Integer' type).
-- This is however not the case in practice as the integer part of any state is
-- at most (k+1).
-- Could I use the 'NAry' type to statically enforce this (and tag a nice 'Finite')
-- instance to the states? Probably, but this also works for now.
pdaEQ ::
  Integer ->
  ProbabilityPDA
    Rational
    (Either Bool (ABC, Integer))
    (Maybe ABC, Integer, Integer)
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
    _startState = Left False
    _finalStates = [Left False, Left True]
    _transRead = \case
      (Left False, A) ->
        let f x = Right (Nothing, x, undefined)
         in f <$> Dist.uniform [1 .. fromIntegral k]
      (Right (A, x), A) ->
        pure $ Right (Just A, x, undefined)
      (Right (A, x), B) ->
        pure $ Right (Just B, x, undefined)
      (Right (B, x), B) ->
        pure $ Right (Just B, x, undefined)
      (Right (B, x), C) ->
        pure $ Right (Just C, x, succ x)
      (Right (C, x), C) ->
        pure $ Right (Just C, x, succ x)
      _ -> Dist.fromFreqs empty
    _transPop = \case
      ((Nothing, x, _), Bottom) ->
        pure $ Left (Right (A, x), [Bottom])
      ((Just A, x, _), t) ->
        pure $ Left (Right (A, x), [SSymbol (), t])
      ((Just B, x, _), t) ->
        pure $ Left (Right (B, x), genericReplicate x (SSymbol ()) <> [t])
      ((Just C, x, y), SSymbol ()) ->
        case pred y of
          0 -> pure $ Left (Right (C, x), [])
          y' -> pure $ Right (Just C, x, y')
      ((Just C, _, _), Bottom) ->
        pure $ Left (Left True, [Bottom])
      c -> error $ "invalid pop state / stack input " ++ show c

{- Bibliography
 - ~~~~~~~~~~~~
 - [1]: Hromkovič, J., & Schnitger, G. (2010). On probabilistic pushdown automata.
 -        Information and Computation, 208(8), 982-995.
-}
