{-# LANGUAGE OverloadedLists #-}

-- | Pushdown automata generalized with a mondaic transition function.
module Automata.PushDown.Monadic where

import Automata.PushDown.Util
import Control.Monad (liftM2)
import Data.Heart
import Data.Maybe (isJust)
import Data.Set (Set)
import qualified Data.Set as Set

-- | A Monadic Pushdown Automaton is a generalization of
-- the Functional Pushdown Automaton and is defined by an 7-tuple
--  (M, Q, Σ, Γ, δ, q_1, F), where
--
--   1. M is a monad.
--   2. Q is a finite set called the *states*,
--   3. Σ is a finite set called the *input alphabet*,
--   4. Γ is a finite set called the *stack alphabet*,
--   5. δ : Q × Γ_ε × Σ_ε → M(Q × Γ<3 × {0,1}) is the *transition function*,
--   6. q_1 ∈ Q is the *start state*, and
--   7. F ⊆ Q is the set of *accepting states*.
--
-- The notation Γ<3 denotes the set {ε} ∪ Γ ∪ (Γ × Γ).
--
-- The monad, states, input- and stack alphabet is implicitly given by the types
-- `m`, `s`, `a` and `t` respectively.
data MonadicPDA m s a t = MonadicPDA
  { -- | The start state q_1.
    start :: s,
    -- | The set of final states F.
    final :: Set s,
    -- | The monadic transition function δ.
    trans :: (s, Maybe t, Maybe a) -> m (s, Heart t, Bool)
  }

-- | Step the PDA from one configuration to the next.
stepM :: (Monad m) => MonadicPDA m s a t -> (s, [t], [a]) -> m (s, [t], [a])
stepM pda (s, ts, as) = do
  let (a', as') = split1 as
      (t', ts') = split1 ts
  (s', us, consume) <- trans pda (s, t', a')
  let ts'' = heartToList us ++ ts'
      as'' = if consume then as' else as
  pure (s', ts'', as'')

-- | Transitively step the PDA until an infinite loop is found.
-- If there is no more input left, returns the states that were
-- reached since last reading input. Otherwise, returns 'Nothing'.
--
-- I think this needs a 'Foldable' instance in addition to the 'Monad' instance.
-- My reasoning is as follows:
--   Suppose our monad is the List monad (treating it as existential non-determinism),
--   and we have a PDA with start state 1, final states {1}, and δ(1, ε, ε) = [].
--   This should clearly accept the empty string, but will end up rejecting it,
--   as 'stepsM' would return an empty list and so we wouldn't know that
--   state 1 is reachable.
--   We can't just append state 1 either, as this has to work for *any* monad!
--   We therefore need to explicitly check if the result is empty,
--   which we can with the 'null' function of the 'Foldable' instance.
--
-- Luckily all of the monads we are interested in
-- ('Identity', 'List', double-nested lists, etc.) are 'Foldable'.
--
-- Another option might be to use 'MonadPlus', but 'Identity'
-- has no such instance. We *could* fix this by using 'Maybe' instead
-- and just treating 'Nothing' as rejection, but that's a little ugly.
stepsM ::
  (Monad m, Foldable m, Ord s, Ord a, Ord t) =>
  MonadicPDA m s a t ->
  Set (s, [t]) ->
  (s, [t], [a]) ->
  m (Set s)
stepsM pda seen c@(s, t, a) =
  -- Before doing any monadic action, we need make sure we return
  -- the current state if there is no input left.
  -- The reason for this is that the monadic action might fail
  -- (e.g. for the List monad the result of 'stepM' might be @[]@),
  -- but even so we still need to return the current state
  -- if there's not more input left.
  --
  -- Therefore we append the @current@ state to the set of further
  -- reachable states, or, if finding the reachable states failed,
  -- we just return the current state.
  if null reachable
    then current
    else liftM2 Set.union current reachable
  where
    current = pure (if null a then [s] else Set.empty)
    reachable = do
      c'@(s', t', a') <- stepM pda c
      -- If we have read an input symbol, we clear the `seen` list,
      -- as we are then in a new configuration.
      -- Otherwise, we add the last state/stack pair to the previously seen ones.
      let seen' = if length a' < length a then [] else Set.insert (s, t) seen
      if isJust $ dejavu seen' (s', t')
        -- If we have already seen a similar state,
        -- we can simply return an empty set as we know
        -- that @s'@ has already been appended the last time we saw it.
        then pure []
        -- If we have not been in a similar state we continue looping.
        else stepsM pda seen' c'

runMPDA ::
  (Monad m, Foldable m, Ord s, Ord a, Ord t) =>
  MonadicPDA m s a t ->
  [a] ->
  m Bool
runMPDA pda as =
  let ss = stepsM pda [] (start pda, [], as)
   in any (`Set.member` final pda) <$> ss
