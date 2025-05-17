{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | Non-deterministic finite automata
module Automata.FiniteState.NFA (NFA (..), accepts, stepE, step, fromDFA, toDFA) where

import Automata.FiniteState.DFA (DFA)
import qualified Automata.FiniteState.DFA as DFA
import Data.Foldable (foldl')
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Universe.Class (Finite (universeF))
import GHC.Generics (Generic)

-- | A non-deterministic finite automaton is a 5-tuple
--  (Q, Σ, δ, q_1, F), where
--
--    1. Q is a finite set called the *states*,
--    2. Σ is a finite set called the *alphabet*,
--    3. δ : Q × Σ_ε → P(Q) is the transition function,
--    4. q_1 ∈ Q is the *start state*, and
--    5. F ⊆ Q is the *set of final states*. [1]
--
-- The states and alphabet is implicitly given by the type.
data NFA a s = NFA
  { -- | The start state q_1.
    start :: s,
    -- | The set of final states F.
    final :: Set s,
    -- | The transition function δ.
    trans :: (s, Maybe a) -> Set s
  }
  deriving (Generic)

-- | The ε-closure of the NFA in state @q@.
stepE :: (Ord s) => NFA a s -> s -> Set s
stepE nfa q = go $ Set.singleton q
  where
    f r = trans nfa (r, Nothing)
    go qs =
      let rs = Set.unions $ Set.map f qs
       in if rs `Set.isSubsetOf` qs
            then qs
            else go (qs <> rs)

-- | Step the NFA in state @q@ with input symbol @x@.
step :: (Ord s) => NFA a s -> s -> a -> Set s
step nfa q x =
  let rs = trans nfa (q, Just x)
   in Set.unions $ Set.map (stepE nfa) rs

-- | Does the NFA accept the input string @xs@?
accepts :: (Ord s) => NFA a s -> [a] -> Bool
accepts nfa xs = any (`Set.member` final nfa) r_n
  where
    r_1 = stepE nfa $ start nfa
    r_n = foldl' f r_1 xs
    f qs x = Set.unions $ Set.map (flip (step nfa) x) qs

-- | Convert a DFA to an equivalent NFA.
fromDFA :: DFA a s -> NFA a s
fromDFA dfa =
  NFA
    { start = DFA.start dfa,
      final = DFA.final dfa,
      trans = delta
    }
  where
    delta (_, Nothing) = Set.empty
    delta (q, Just x) = Set.singleton $ DFA.trans dfa (q, x)

-- | Convert a NFA to an equivalent DFA.
toDFA :: (Ord s, Finite s) => NFA a s -> DFA a (Set s)
toDFA nfa =
  DFA.DFA
    { DFA.start = _start,
      DFA.final = _final,
      DFA.trans = delta
    }
  where
    _start = stepE nfa $ start nfa
    _final = Set.filter (any (`Set.member` final nfa)) qs
    qs = Set.powerSet $ Set.fromList universeF
    delta (rs, x) =
      let d r = step nfa r x
       in Set.unions $ Set.map d rs

{- Bibliography
 - ~~~~~~~~~~~~
 - [1]: M. Sipser, Introduction to the theory of computation, Third edition. Cengage Learning, 2013.
-}
