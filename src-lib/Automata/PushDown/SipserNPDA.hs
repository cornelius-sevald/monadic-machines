{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TupleSections #-}

-- | Sipser Non-deterministic Pushdown Automata.
module Automata.PushDown.SipserNPDA where

import Automata.PushDown.Util
import Data.Maybe (isNothing)
import Data.Set (Set)
import qualified Data.Set as Set

-- | A Sipser Non-deterministic Pushdown Automaton (SipserNPDA)
-- is a 6-tuple (Q, Σ, Γ, δ, q_1, F), where
--
--    1. Q is a finite set called the *states*,
--    2. Σ is a finite set called the *input alphabet*,
--    2. Γ is a finite set called the *stack alphabet*,
--    3. δ : Q × Γ_ε × Σ_ε → P(Q × Γ^*) is the transition function,
--    4. q_1 ∈ Q is the *start state*, and
--    5. F ⊆ Q is the *set of final states*. [1]
--
-- In the book, the transition function may only push zero or
-- one stack symbols, but allowing it to push an arbitrary
-- amount doesn't change its power and makes conversions
-- to other types of NPDAs much simpler.
--
-- The states, input- and stack alphabet is implicitly given by the types
-- `s`, `a` and `t` respectively.
data SipserNPDA s a t = SipserNPDA
  { -- | The start state q_1.
    startState :: s,
    -- | The set of final states F.
    finalStates :: Set s,
    -- | The transition function δ.
    trans :: (s, Maybe t, Maybe a) -> Set (s, [t])
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
      pure (s', t' ++ ts')

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
        else Set.foldl (stepE pda) seen' new

-- | Version of 'stepE' which also keeps tracks if a final
-- state was reached on the path to each configuration.
stepE' ::
  (Ord s, Ord t) =>
  -- | The PDA.
  SipserNPDA s a t ->
  -- | A set of state/stack configurations we have previously seen.
  Set ((s, [t]), Bool) ->
  -- | The current state/stack configuration.
  ((s, [t]), Bool) ->
  -- | A set of new configurations.
  Set ((s, [t]), Bool)
stepE' pda seen ((s, ts), b) =
  let isFinal = s `Set.member` finalStates pda
      b' = b || isFinal
      seen' = Set.insert ((s, ts), b') seen
      -- We step with no input,
      -- yielding a set of successor configurations.
      cs' = stepStack pda s ts Nothing
      -- We filter out the already-seen configurations
      -- (or at least those similar enough),
      -- leaving only new configurations.
      new = Set.filter (isNothing . dejavu (Set.map fst seen')) cs'
   in if null new
        -- If there are no new successor configurations,
        -- then we return the seen configurations.
        then seen'
        -- Otherwise we recursively step from the new configurations.
        else Set.foldl (stepE' pda) seen' (Set.map (,b') new)

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
accepts pda as = go as (startState pda, [])
  where
    go [] c =
      let ss = Set.map fst $ stepE pda [] c
       in not $ null $ Set.intersection ss (finalStates pda)
    go (a : as') c =
      let cs' = step pda a c
       in any (go as') cs'

-- A type of Sipser NPDA with a dedicated end-of-input symbol.
type EOISipserNPDA s a t = SipserNPDA s (Ended a) t

-- | Wrapper acceptance function for Sipser NPDAs with a dedicated end-of-input symbol,
-- likely created via 'Automata.PushDown.Monadic.List.toSipserNPDA.
--
-- Automatically ends the input word with the dedicated end-of-input symbol.
acceptsEOI :: (Ord s, Ord t) => EOISipserNPDA s a t -> [a] -> Bool
acceptsEOI pda w = accepts pda (fmap ISymbol w <> [End])

{- Bibliography
 - ~~~~~~~~~~~~
 - [1]: M. Sipser, Introduction to the theory of computation, Third edition. Cengage Learning, 2013.
-}
