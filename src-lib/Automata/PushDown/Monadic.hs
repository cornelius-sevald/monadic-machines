{-# LANGUAGE OverloadedLists #-}

-- | Pushdown automata generalized with a mondaic transition function.
module Automata.PushDown.Monadic where

import Automata.PushDown.Util
import Control.Monad (foldM)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Set (Set)
import qualified Data.Set as Set

-- | A Monadic Pushdown Automaton (MPDA) is a generalization
-- of the Functional Pushdown Automaton, and is defined by a
-- 9-tuple (M, Q, Σ, Γ, δ, γ, Z_1, q_1, F) where,
--
--   1. M is a monad.
--   2. Q is a finite set called the *states*,
--   3. Σ is a finite set called the *input alphabet*,
--   4. Γ is a finite set called the *stack alphabet*,
--   5. δ : Q × Γ × Σ → M(Q × Γ^* × {0,1}) is the *input transition function*,
--   6. γ : Q × Γ → M(Q) is the *stack transition function*,
--   7. Z_1 ∈ Γ is the *start stack symbol*, and
--   8. q_1 ∈ Q is the *start state*, and
--   9. F ⊆ Q is the set of *accepting states*.
--
-- The notation Γ^* denotes the set {ε} ∪ Γ ∪ (Γ × Γ) ∪ ....
--
-- The monad, states, input- and stack alphabet is implicitly given by the types
-- `m`, `s`, `a` and `t` respectively.
data MonadicPDA m s a t = MonadicPDA
  { startSymbol :: t,
    startState :: s,
    finalStates :: Set s,
    transInput :: (s, t, a) -> m (s, [t], Bool),
    transStack :: (s, t) -> m s
  }

-- | Step the MPDA from one configuration to the next,
-- with input symbol @a@ and stack symbol @t@.
stepInputM ::
  (Monad m) =>
  MonadicPDA m s a t ->
  (s, NonEmpty t, NonEmpty a) ->
  m (s, [t], [a])
stepInputM m (s, t :| ts, a :| as) = do
  (s', ts', consume) <- transInput m (s, t, a)
  let as' = if consume then as else a : as
  pure (s', ts' <> ts, as')

-- | Transitively step the MPDA
stepsInputM ::
  (Monad m, Eq s, Eq a, Eq t) =>
  MonadicPDA m s a t ->
  [(s, [t])] ->
  (s, [t], [a]) ->
  m (Maybe (s, [t]))
stepsInputM _ _ (s, ts, []) = pure $ Just (s, ts)
stepsInputM _ _ (_, [], _) = pure Nothing
stepsInputM m seen (s, t : ts, a : as) =
  case dejavu seen (s, t : ts) of
    Just _ -> pure Nothing
    Nothing -> do
      (s', ts', as') <- stepInputM m (s, t :| ts, a :| as)
      let seen' = if as' == (a : as) then (s, t : ts) : seen else []
      stepsInputM m seen' (s', ts', as')

stepStackM :: (Monad m) => MonadicPDA m s a t -> s -> t -> m s
stepStackM m s t = transStack m (s, t)

stepsStackM :: (Monad m) => MonadicPDA m s a t -> s -> [t] -> m s
stepsStackM m = foldM (stepStackM m)

runMPDA :: (Monad m, Ord s, Eq a, Eq t) => MonadicPDA m s a t -> [a] -> m Bool
runMPDA m as = do
  c <- stepsInputM m [] (startState m, [startSymbol m], as)
  case c of
    Nothing -> pure False
    Just (s', ts') -> do
      s'' <- stepsStackM m s' ts'
      pure (s'' `Set.member` finalStates m)
