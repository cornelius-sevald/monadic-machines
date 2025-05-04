{-# LANGUAGE LambdaCase #-}

-- | Monadic pushdown automaton with the `Probability` monad.
-- Equivalent to an probabilistic pushdown automaton.
module Automata.PushDown.Monadic.Probability
  ( ProbabilityPDA,
    accepts,
    invert,
    concatenateMarked,
  )
where

import Automata.PushDown.Monadic
import Control.Arrow
import qualified Data.Set as Set
import Data.Universe.Class (Finite (universeF))
import Numeric.Probability.Distribution (T)
import qualified Numeric.Probability.Distribution as Dist

type ProbabilityPDA prob r p a t = MonadicPDA (T prob) r p a t

-- | Acceptance function for PPDA.
--
-- Takes a *cut-point* 0 ≤ η < 1 which is the threshold
-- at which words are accepted, i.e. a larger value
-- of η means that the automaton is less likely to accepts.
--
-- NOTE: If running the PDA results in an "empty" probability,
-- this will throw an error. However, you should not make empty
-- transitions in the probability monad anyway, as it results
-- in invalid probabilities when running the PDA, i.e. you may
-- get results where P(accept) + P(reject) < 1.
accepts ::
  (Ord r, Ord prob, Num prob) =>
  ProbabilityPDA prob r p a t ->
  prob ->
  [a] ->
  Bool
accepts m η w = acceptance $ runMPDA m w
  where
    acceptance t = Dist.truth t > η

-- | Invert PPDA M, such that `invert M` accepts a word
-- iff. M rejects that word.
--
-- TODO: Test
invert ::
  (Finite r, Ord r) =>
  ProbabilityPDA prob r p a t ->
  ProbabilityPDA prob r p a t
invert m = m {finalStates = complement $ finalStates m}
  where
    complement = (Set.fromList universeF `Set.difference`)

-- | Concatenate two PPDAs M1 and M2 with a dediacted
-- marker symbol `#` between, such that `concatenateMarked M1 M2`
-- recognizes the language { x#y | x ∈ Σ*, y ∈ Σ*, M1 accepts x, M2 accepts y }.
--
-- While the CFL is closed under concatenation,
-- I don't believe that this is the case for the stochastic CFL,
-- and so we would need a dedicated marker symbol.
--
-- TODO: Test
concatenateMarked ::
  (Num prob, Ord r1, Ord r2) =>
  ProbabilityPDA prob r1 p1 a t1 ->
  ProbabilityPDA prob r2 p2 a t2 ->
  ProbabilityPDA
    prob
    (Either r1 r2)
    (Maybe (Either p1 p2))
    (Maybe a)
    (Either t1 t2)
concatenateMarked m1 m2 =
  MonadicPDA
    { startSymbol = _startSymbol,
      startState = _startState,
      finalStates = _finalStates,
      transRead = _transRead,
      transPop = _transPop
    }
  where
    _startSymbol = Left $ startSymbol m1
    _startState = Left $ startState m1
    _finalStates = Set.map Right (finalStates m2)
    _transRead = \case
      -- In a read state from M1 with input a, simulate δ_read of M1.
      (Left r, Just a) -> do
        res <- transRead m1 (r, a)
        let wrap = (Left *** fmap Left) +++ (Just . Left)
        pure $ wrap res
      -- In a read state from M2 with input a, simulate δ_read of M2.
      (Right r, Just a) -> do
        res <- transRead m2 (r, a)
        let wrap = (Right *** fmap Right) +++ (Just . Right)
        pure $ wrap res
      -- In a read state from M1 with marker symbol,
      -- go to the start state of M2 with its start symbol
      -- iff. the read state is a final state.
      -- Otherwise, reject the input.
      (Left r, Nothing)
        | r `Set.member` finalStates m1 -> do
            let q = Right $ startState m2
                ts = [Right $ startSymbol m2]
            pure $ Left (q, ts)
        | otherwise -> pure dead
      -- In a read state from M2 with the marker symbol,
      -- we reject the input.
      (Right _, Nothing) -> pure dead
    _transPop = \case
      -- In a pop state from M1 with stack symbol from M1.
      -- We simulate δ_pop of M1.
      (Just (Left p), Left t) -> do
        res <- transPop m1 (p, t)
        let wrap = (Left *** fmap Left) +++ (Just . Left)
        pure $ wrap res
      -- In a pop state from M2 with stack symbol from M2.
      -- We simulate δ_pop of M2.
      (Just (Right p), Right t) -> do
        res <- transPop m2 (p, t)
        let wrap = (Right *** fmap Right) +++ (Just . Right)
        pure $ wrap res
      -- In a pop state from M1 with stack symbol from M2.
      -- This should be impossible, so we raise an exception
      -- if it somehow does happen.
      (Just (Left _), Right _) ->
        let msg =
              "concatenateMarked:"
                <> " In pop state of M1 with stack symbol from M2."
                <> " This should not be possible."
         in error msg
      -- In a pop state from M2 with stack symbol from M1.
      -- This means we have effectively reached the bottom
      -- of the stack of M2, and so we reject the input.
      (Just (Right _), Left _) -> pure dead
      -- A dead state.
      (Nothing, _) -> pure dead
    dead = Right Nothing
