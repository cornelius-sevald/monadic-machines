{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | Deterministic finite automata
module Automata.DFA (DFA (..), step) where

import qualified Automata.Class
import Data.Set (Set)
import qualified Data.Set as Set

-- | A deterministic finite automaton is a 5-tuple
--  ($Q, \Sigma, \delta, q_0, F$), where
--
--    1. $Q$ is a finite set called the *states*,
--    2. $\Sigma$ is a finite set called the *alphabet*,
--    3. $\delta : Q \times \Sigma \rightarrow Q$ is the transition function,
--    4. $q_0 \in Q$ is the *start state*, and
--    5. $F \subseteq Q$ is the *set of final states*. [@sipser_introduction_2013]
--
-- The states and alphabet is implicitly given by the type.
data DFA a s = DFA
  { -- | The transition function $\delta$.
    trans :: (s, a) -> s,
    -- | The start state $q_0$.
    start :: s,
    -- | The set of final states $F$.
    final :: Set s
  }

-- | Step the DFA in state @q@ with input symbol @x@.
step :: DFA a s -> s -> a -> s
step dfa q x = trans dfa (q, x)

-- | Does the DFA @m@ accept the input string @xs@?
accepts :: (Ord s) => DFA a s -> [a] -> Bool
accepts m xs = r_n `Set.member` final m
  where
    r_0 = start m
    r_n = foldl (step m) r_0 xs

instance (Ord s) => Automata.Class.Acceptor DFA a s where
  accepts = accepts
