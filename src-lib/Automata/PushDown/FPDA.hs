-- | 2-stack Deterministic Pushdown Automata.
module Automata.PushDown.FPDA where

import Data.Heart
import Data.Maybe (listToMaybe)
import Data.Set (Set)
import qualified Data.Set as Set

-- | A 2-stack Pushdown Automaton (2sDPDA), is a 6-tuple (Q, Σ, Γ, δ, q_1, F) where,
--
--   1. Q is a finite set called the *states*,
--   2. Σ is a finite set called the *input alphabet*,
--   3. Γ is a finite set called the *stack alphabet*,
--   4. δ : Q × Γ_ε × Σ_ε → Q × Γ<3 × {0,1} is the *transition function*,
--   5. q_1 ∈ Q is the *start state*, and
--   6. F ⊆ Q is the set of *accepting states*.
--
-- Modified from the definition in [1].
--
-- The notation Γ<3 denotes the set {ε} ∪ Γ ∪ (Γ × Γ).
--
-- The boolean value returned by the transition function indicates
-- whether or not the input symbol is consumed or not,
-- i.e. if we pop the symbol from the input list or merely peek.
--
-- The states, input- and stack alphabet is implicitly given by the types
-- `s`, `a` and `t` respectively.
data FPDA s a t = FPDA
  { start :: s,
    final :: Set s,
    trans :: (s, Maybe t, Maybe a) -> (s, Heart t, Bool)
  }

split :: [x] -> (Maybe x, [x])
split [] = (Nothing, [])
split (x : xs) = (Just x, xs)

-- | Check if we have been in a similar configuration before.
-- Specifically, check if we have been in a configuration
-- with the same state, same top of the stack, and not a smaller stack.
--
-- This is equivalent to the `existsIn` function from [2].
dejavu :: (Eq s, Eq t) => [(s, [t])] -> (s, [t]) -> Bool
dejavu before now = any (been now) before
  where
    been (s, ts) (q, ys) =
      -- States match,
      s == q
        -- and the top of the stacks match,
        && listToMaybe ts == listToMaybe ys
        -- and the `now` stack has not shrunk.
        && length ts >= length ys

-- | Step the 2sDPDA from one configuration to the next.
--
-- The implementation is based on the `transitionFPDA` from [1].
step :: (Eq a) => FPDA s a t -> (s, [t], [a]) -> (s, [t], [a])
step dpda (s, ts, as) =
  let (a', as') = split as
      (t', ts') = split ts
      (s', us, consume) = trans dpda (s, t', a')
      ts'' = heartToList us ++ ts'
      as'' = if consume then as' else as
   in (s', ts'', as'')

-- | Transitively step the 2sDPDA until an infinite loop is found.
--
-- As each state has exactly one successor,
-- we will always end up in an infinite loop.
-- We then return the list of states that were reached
-- since last reading a new input symbol, as well as the remaining input
--
-- The implementation is based on the `stepsFPDA` from [1],
-- but I found that it can loop forever by bouncing between
-- two different states when there is no input left.
steps ::
  (Eq s, Eq a, Eq t) =>
  FPDA s a t ->
  [(s, [t])] ->
  (s, [t], [a]) ->
  ([s], [a])
steps dpda seen c@(s, t, a) =
  let c'@(s', t', a') = step dpda c
      -- If we have read an input symbol, we clear the `seen` list,
      -- as we are then in a new configuration.
      -- Otherwise, we add the last state/stack pair to the previously seen ones.
      seen' = if length a' < length a then [] else (s, t) : seen
   in if dejavu seen' (s', t')
        then (fst <$> seen', a')
        else steps dpda seen' c'

accepts :: (Ord s, Eq a, Eq t) => FPDA s a t -> [a] -> Bool
accepts dpda as =
  let (ss, a') = steps dpda [] (start dpda, [], as)
   in -- We accept if we have read all input,
      -- and any of the reachable states from there is a final one.
      null a' && any (`Set.member` final dpda) ss

{- Bibliography
 - ~~~~~~~~~~~~
 - [1]: T. D. Randløv, “Toward a Monadic Functional Machine Model for Computability and Complexity Theory: Finite and Pushdown Automata,” Master’s Thesis, University of Copenhagen, 2023.
-}
