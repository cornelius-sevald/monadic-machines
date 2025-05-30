{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TupleSections #-}

-- | Non-deterministic Pushdown Automata.
module Automata.PushDown.NPDA where

import Automata.PushDown.Util
import Control.Arrow (Arrow ((***)))
import Data.Maybe (isNothing)
import Data.Set (Set)
import qualified Data.Set as Set

-- | A non-deterministic Pushdown Automaton (NPDA)
-- is a 6-tuple (Q, Σ, Γ, δ, q_1, F), where
--
--    1. Q is a finite set called the *states*,
--    2. Σ is a finite set called the *input alphabet*,
--    2. Γ is a finite set called the *stack alphabet*,
--    3. δ : Q × Γ_ε × Σ_ε → P(Q × Γ^*) is the transition function,
--    4. q_1 ∈ Q is the *start state*, and
--    5. F ⊆ Q is the *set of final states*. [1]
--
-- In [1], the transition function may only push zero or
-- one stack symbols, but allowing it to push an arbitrary
-- amount doesn't change its power and makes conversions
-- to other types of NPDAs much simpler.
--
-- The states, input- and stack alphabet is implicitly given by the types
-- `s`, `a` and `t` respectively.
data NPDA s a t = NPDA
  { -- | The start state q_1.
    startState :: s,
    -- | The set of final states F.
    finalStates :: Set s,
    -- | The transition function δ.
    trans :: (s, Maybe t, Maybe a) -> Set (s, [t])
  }

-- | Perform a single frame-rule.
-- Computes all β that satisfy (ts, q, a) ⊢ β.
stepFrame ::
  (Ord s, Ord t) =>
  NPDA s a t ->
  (s, [t]) ->
  Maybe a ->
  Set (s, [t])
stepFrame pda (q, ts) a =
  Set.unions $ go <$> split01 ts
  where
    go (t, ts') = Set.fromList $ do
      (q', t') <- Set.toList $ trans pda (q, t, a)
      pure (q', t' ++ ts')

-- | Compute the ε-closure of this state/stack,
-- i.e. the set of configurations reachable via only ε-transitions
-- whose path don't involve a non-descending ε-cycle.
stepE ::
  (Ord s, Ord t) =>
  -- | The PDA.
  NPDA s a t ->
  -- | The current state/stack configuration.
  (s, [t]) ->
  -- | A set of new configurations.
  Set (s, [t])
stepE pda = go []
  where
    go seen (s, ts) =
      let seen' = Set.insert (s, ts) seen
          -- We step with no input,
          -- yielding a set of successor configurations.
          cs' = stepFrame pda (s, ts) Nothing
          -- We filter out the already-seen configurations
          -- (or at least those similar enough),
          -- leaving only new configurations.
          new = Set.filter (isNothing . dejavu seen') cs'
       in -- We return the current configuration,
          -- and recursively step the new ones.
          Set.singleton (s, ts) `Set.union` Set.unions (Set.map (go seen') new)

-- | Version of 'stepE' which also keeps a trace if a final
-- state was reached on the path to each configuration.
stepTraceE ::
  (Ord s, Ord t) =>
  -- | The PDA.
  NPDA s a t ->
  -- | The current state/stack configuration.
  (s, [t]) ->
  -- | A set of new configurations.
  Set ((s, [t]), Bool)
stepTraceE pda c = go [] (c, False)
  where
    go seen ((s, ts), b) =
      let isFinal = s `Set.member` finalStates pda
          b' = b || isFinal
          x' = ((s, ts), b')
          seen' = Set.insert x' seen
          -- We step with no input,
          -- yielding a set of successor configurations.
          cs' = stepFrame pda (s, ts) Nothing
          -- We filter out the already-seen configurations
          -- (or at least those similar enough),
          -- leaving only new configurations.
          new = Set.map (,b') $ Set.filter (isNothing . dejavu (Set.map fst seen')) cs'
       in -- We return the current configuration,
          -- and recursively step the new ones.
          Set.singleton x' `Set.union` Set.unions (Set.map (go seen') new)

-- | Step using the frame rule until the input symbol 'a' has been read.
-- Computes the set of configurations { α_1, α_2, ... , α_n } ∪ { β }
-- such that α_i = (t_i, q_i, a), β = (t_{n+1}, q_{n+1}, ε), and
--   α_1 ⊢ α_2 ⊢ ... ⊢ α_n ⊢ β.
step :: (Ord s, Ord t) => NPDA s a t -> a -> (s, [t]) -> Set (s, [t])
step pda a (s, ts) =
  -- We get the set of configurations reachable
  -- without consuming any input.
  let cs' = stepE pda (s, ts)
   in -- And then step each of those with the input.
      Set.unions $ Set.map (\c -> stepFrame pda c (Just a)) cs'

-- | The PDA accepts a word iff. there exists a
-- path from the start state to a final state after
-- reading the entire input,
-- *and this path contains no non-decreasing ε-cycles*.
accepts :: (Ord s, Ord t) => NPDA s a t -> [a] -> Bool
accepts pda as = go as (startState pda, [])
  where
    go [] c =
      let qs = Set.map fst $ stepE pda c
       in not $ null $ Set.intersection qs (finalStates pda)
    go (a : as') c =
      let cs' = step pda a c
       in any (go as') cs'

-- | For a NPDA M1 and M2 both with alphabet Σ,
-- construct a new NPDA M such that, for string w ∈ Σ*,
-- M accepts w iff. either M1 or M2 (or both) accepts w.
union ::
  (Ord s1, Ord s2, Ord t1, Ord t2) =>
  NPDA s1 a t1 ->
  NPDA s2 a t2 ->
  NPDA (Maybe (Either s1 s2)) a (Either t1 t2)
union m1 m2 =
  NPDA
    { startState = _startState,
      finalStates = _finalStates,
      trans = _trans
    }
  where
    _startState = Nothing
    _finalStates =
      Set.map lef (finalStates m1)
        `Set.union` Set.map rig (finalStates m2)
    _trans = \case
      (Nothing, Nothing, Nothing) ->
        let l = (lef $ startState m1, [])
            r = (rig $ startState m2, [])
         in [l, r]
      (Just (Left q), Nothing, a) -> transL (q, Nothing, a)
      (Just (Left q), Just (Left t), a) -> transL (q, Just t, a)
      (Just (Right q), Nothing, a) -> transR (q, Nothing, a)
      (Just (Right q), Just (Right t), a) -> transR (q, Just t, a)
      _ -> []
    lef = Just . Left
    rig = Just . Right
    transL = Set.map (lef *** fmap Left) . trans m1
    transR = Set.map (rig *** fmap Right) . trans m2

{- Bibliography
 - ~~~~~~~~~~~~
 - [1]: M. Sipser, Introduction to the theory of computation, Third edition. Cengage Learning, 2013.
-}
