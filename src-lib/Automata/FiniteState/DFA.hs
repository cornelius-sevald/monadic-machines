{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | Deterministic finite automata
module Automata.FiniteState.DFA (DFA (..), accepts, step) where

import Data.Set (Set)
import qualified Data.Set as Set
import GHC.Generics

-- | A deterministic finite automaton is a 5-tuple
--  (Q, Σ, δ, q_1, F), where
--
--    1. Q is a finite set called the *states*,
--    2. Σ is a finite set called the *alphabet*,
--    3. δ : Q × Σ → Q is the transition function,
--    4. q_1 ∈ Q is the *start state*, and
--    5. F ⊆ Q is the *set of final states*. [1]
--
-- The states and alphabet is implicitly given by the type.
data DFA a s = DFA
  { -- | The start state q_1.
    start :: s,
    -- | The set of final states F.
    final :: Set s,
    -- | The transition function δ.
    trans :: (s, a) -> s
  }
  deriving (Generic)

-- | Step the DFA in state @q@ with input symbol @x@.
step :: DFA a s -> s -> a -> s
step dfa q x = trans dfa (q, x)

-- | Does the DFA @m@ accept the input string @w@?
accepts :: (Ord s) => DFA a s -> [a] -> Bool
accepts m w = r_n `Set.member` final m
  where
    r_0 = start m
    r_n = foldl (step m) r_0 w

{- Bibliography
 - ~~~~~~~~~~~~
 - [1]: M. Sipser, Introduction to the theory of computation, Third edition. Cengage Learning, 2013.
-}
