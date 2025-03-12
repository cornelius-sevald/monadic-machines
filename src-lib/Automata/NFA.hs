{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | Non-deterministic finite automata
module Automata.NFA where

import Automata.DFA (DFA)
import qualified Automata.DFA as DFA
import qualified Automata.FiniteAutomaton as FA
import Control.Monad (foldM)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Universe.Class (Finite (universeF))

-- | A non-deterministic finite automaton is a 5-tuple
--  ($Q, \Sigma, \delta, q_0, F$), where
--
--    1. $Q$ is a finite set called the *states*,
--    2. $\Sigma$ is a finite set called the *alphabet*,
--    3. $\delta : Q \times \Sigma_\epsilon \rightarrow \mathcal{P}(Q)$ is the transition function,
--    4. $q_0 \in Q$ is the *start state*, and
--    5. $F \subseteq Q$ is the *set of final states*. [@sipser_introduction_2013]
--
-- The states and alphabet is implicitly given by the type.
data NFA a s = NFA
  { -- | The transition function $\delta$.
    trans :: (s, Maybe a) -> [s],
    -- | The start state $q_0$.
    start :: s,
    -- | The set of final states $F$.
    final :: Set s
  }

-- | Step the NFA in state @q@ with no input symbols.
stepE :: (Ord s) => NFA a s -> s -> [s]
stepE nfa q = step1 ++ stepMany
  where
    step1 = trans nfa (q, Nothing)
    stepMany = concatMap (stepE nfa) (trans nfa (q, Nothing))

-- | Step the NFA in state @q@ with input symbol @x@.
step :: (Ord s) => NFA a s -> s -> a -> [s]
step nfa q x =
  case stepE nfa q of
    [] -> step1 q
    (r : rs) -> step1 q ++ step1 r ++ stepMany rs
  where
    step1 r = trans nfa (r, Just x)
    stepMany = concatMap (flip (step nfa) x)

-- | Does the NFA @m@ accept the input string @xs@?
accepts :: (Ord s) => NFA a s -> [a] -> Bool
accepts m xs = any (`Set.member` final m) r_n
  where
    r_0 = start m
    r_n = foldM (step m) r_0 xs

-- | Convert a DFA to an equivalent NFA.
fromDFA :: DFA a s -> NFA a s
fromDFA dfa =
  NFA
    { start = DFA.start dfa,
      final = DFA.final dfa,
      trans = delta
    }
  where
    delta (_, Nothing) = []
    delta (q, Just x) = [DFA.trans dfa (q, x)]

-- | [WIP] Convert a NFA to an equivalent DFA.
-- TODO: Also handle epsilon transitions.
toDFA :: (Ord s, Finite s) => NFA a s -> DFA a (Set s)
toDFA nfa =
  DFA.DFA
    { DFA.start = Set.singleton $ start nfa,
      DFA.final = fs,
      DFA.trans = delta
    }
  where
    qs = Set.powerSet $ Set.fromList universeF
    fs = Set.filter (any (`Set.member` final nfa)) qs
    delta (rs, x) =
      let d r = Set.fromList $ trans nfa (r, Just x)
       in Set.unions $ Set.map d rs

instance (Ord s) => FA.Acceptor NFA a s where
  accepts = accepts
