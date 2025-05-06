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

-- | Concatenate two PPDAs M1 and M2 with a dedicated
-- marker symbol `#` between, such that `concatenateMarked M1 M2`
-- recognizes, with cut-point η the language
-- { x#y | x ∈ Σ*
--       , y ∈ Σ*
--       , M1 accepts x w. cut-point η₁
--       , M2 accepts y w. cut point η₂
--       , η ≤ η₁ * η₂
-- }
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
      (Left r, Just a) -> wrapL <$> transRead m1 (r, a)
      -- In a read state from M2 with input a, simulate δ_read of M2.
      (Right r, Just a) -> wrapR <$> transRead m2 (r, a)
      -- In a read state from M1 with marker symbol,
      -- go to the start state of M2 with its start symbol
      -- iff. the read state is a final state.
      -- Otherwise, reject the input.
      (Left r, Nothing)
        | r `Set.member` finalStates m1 ->
            let q = startState m2
                ts = [startSymbol m2]
             in pure $ wrapR $ Left (q, ts)
        | otherwise -> pure dead
      -- In a read state from M2 with the marker symbol,
      -- we reject the input.
      (Right _, Nothing) -> pure dead
    _transPop = \case
      -- In a pop state from M1 with stack symbol from M1.
      -- We simulate δ_pop of M1.
      (Just (Left p), Left t) -> wrapL <$> transPop m1 (p, t)
      -- In a pop state from M2 with stack symbol from M2.
      -- We simulate δ_pop of M2.
      (Just (Right p), Right t) -> wrapR <$> transPop m2 (p, t)
      -- In a pop state from M1 with stack symbol from M2.
      -- This should be impossible, so we raise an exception
      -- if it somehow does happen.
      (Just (Left _), Right _) ->
        let msg =
              "Automata.PushDown.Monadic.Probability.concatenateMarked:"
                <> " In pop state of M1 with stack symbol from M2."
                <> " This should not be possible."
         in error msg
      -- In a pop state from M2 with stack symbol from M1.
      -- This means we have effectively reached the bottom
      -- of the stack of M2, and so we reject the input.
      (Just (Right _), Left _) -> pure dead
      -- A dead state.
      (Nothing, _) -> pure dead
    wrapL = (Left *** fmap Left) +++ (Just . Left)
    wrapR = (Right *** fmap Right) +++ (Just . Right)
    dead = Right Nothing
