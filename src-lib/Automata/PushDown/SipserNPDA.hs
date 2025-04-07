{-# LANGUAGE OverloadedLists #-}

-- | Sipser Non-deterministic Pushdown Automata.
module Automata.PushDown.SipserNPDA where

import Automata.PushDown.Util
import Data.Maybe (isNothing, maybeToList)
import Data.Set (Set)
import qualified Data.Set as Set

-- | A Sipser Non-deterministic Pushdown Automaton (SipserNPDA)
-- is a 6-tuple (Q, Σ, Γ, δ, q_1, F), where
--
--    1. Q is a finite set called the *states*,
--    2. Σ is a finite set called the *input alphabet*,
--    2. Γ is a finite set called the *stack alphabet*,
--    3. δ : Q × Γ_ε × Σ_ε → P(Q × Γ_ε) is the transition function,
--    4. q_1 ∈ Q is the *start state*, and
--    5. F ⊆ Q is the *set of final states*. [1]
--
-- The states, input- and stack alphabet is implicitly given by the types
-- `s`, `a` and `t` respectively.
data SipserNPDA s a t = SipserNPDA
  { -- | The start state q_1.
    start :: s,
    -- | The set of final states F.
    final :: Set s,
    -- | The transition function δ.
    trans :: (s, Maybe t, Maybe a) -> Set (s, Maybe t)
  }

-- | Perform a step in state `s` with (optional) input symbol `a`.
stepStack ::
  (Ord s, Ord t) =>
  SipserNPDA s a t ->
  s ->
  [t] ->
  Maybe a ->
  Set (s, [t])
stepStack pda s ts a =
  Set.unions $ go <$> split01 ts
  where
    go (t, ts') = Set.fromList $ do
      (s', t') <- Set.toList $ trans pda (s, t, a)
      pure (s', maybeToList t' ++ ts')

-- | Perform a step without consuming any input.
stepE ::
  (Ord s, Ord t) =>
  -- | The PDA.
  SipserNPDA s a t ->
  -- | A set of state/stack configurations we have previously seen.
  Set (s, [t]) ->
  -- | The current state/stack configuration.
  (s, [t]) ->
  -- | A set of new configurations.
  Set (s, [t])
stepE pda seen c@(s, ts) =
  let seen' = Set.insert c seen
      -- We step with no input,
      -- yielding a set of successor configurations.
      cs' = stepStack pda s ts Nothing
      -- We filter out the already-seen configurations
      -- (or at least those similar enough),
      -- leaving only new configurations.
      new = Set.filter (isNothing . dejavu seen') cs'
   in if null new
        -- If there are no new successor configurations,
        -- then we return the seen configurations.
        then seen'
        -- Otherwise we recursively step from the new configurations.
        else Set.unions $ Set.map (stepE pda seen') new

-- | Perform a step on a single input symbol.
step :: (Ord s, Ord t) => SipserNPDA s a t -> a -> (s, [t]) -> Set (s, [t])
step pda a (s, ts) =
  -- We get the set of configurations reachable
  -- without consuming any input.
  let cs' = stepE pda [] (s, ts)
   in -- And then step each of those with the input.
      Set.unions $ Set.map f cs'
  where
    f (s', ts') = stepStack pda s' ts' (Just a)

accepts :: (Ord s, Ord t) => SipserNPDA s a t -> [a] -> Bool
accepts pda as = go as (start pda, [])
  where
    go [] c =
      let ss = Set.map fst $ stepE pda [] c
       in not $ null $ Set.intersection ss (final pda)
    go (a : as') c =
      let cs' = step pda a c
       in any (go as') cs'

{- Bibliography
 - ~~~~~~~~~~~~
 - [1]: M. Sipser, Introduction to the theory of computation, Third edition. Cengage Learning, 2013.
-}
