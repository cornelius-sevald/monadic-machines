{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | Non-deterministic finite automata
module Automata.NFA (NFA (..), closureE, step, fromDFA, toDFA) where

import qualified Automata.Class
import Automata.DFA (DFA)
import qualified Automata.DFA as DFA
import Data.Foldable (foldl')
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Universe.Class (Finite (universeF))
import GHC.Generics (Generic)

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
  { -- | The start state $q_0$.
    start :: s,
    -- | The set of final states $F$.
    final :: Set s,
    -- | The transition function $\delta$.
    trans :: (s, Maybe a) -> [s]
  }
  deriving (Generic)

-- | The ε-closure of the NFA in state @q@.
closureE :: (Ord s) => NFA a s -> s -> Set s
closureE nfa q = go $ Set.singleton q
  where
    stepE r = trans nfa (r, Nothing)
    go qs =
      let rs = Set.fromList (concatMap stepE qs)
       in if rs `Set.isSubsetOf` qs
            then qs
            else go (qs <> rs)

-- | Step the NFA in state @q@ with input symbol @x@.
step :: (Ord s) => NFA a s -> s -> a -> Set s
step nfa q x =
  let rs = closureE nfa q
   in Set.fromList $ concatMap step1 rs
  where
    step1 r = trans nfa (r, Just x)

-- | Does the NFA accept the input string @xs@?
-- TODO: Test that this works.
accepts :: (Ord s) => NFA a s -> [a] -> Bool
accepts nfa xs = any (`Set.member` final nfa) r_n
  where
    r_0 = closureE nfa $ start nfa
    r_n' = foldl' f r_0 xs
    r_n = Set.unions $ Set.map (closureE nfa) r_n'
    f qs x = Set.unions $ Set.map (step' x) qs
    step' x q = step nfa q x

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

-- | Convert a NFA to an equivalent DFA.
-- TODO: Test that this works.
toDFA :: (Ord s, Finite s) => NFA a s -> DFA a (Set s)
toDFA nfa =
  DFA.DFA
    { DFA.start = closureE nfa (start nfa),
      DFA.final = fs,
      DFA.trans = delta
    }
  where
    qs = Set.powerSet $ Set.fromList universeF
    fs = Set.filter (any (`Set.member` prefinal)) qs
    delta (rs, x) =
      let d r = step nfa r x
       in Set.unions $ Set.map d rs
    -- All states that can reach a final state with no input.
    prefinal = Set.fromList [q | q <- universeF, intersects (closureE nfa q) (final nfa)]
    intersects r u = not $ Set.null $ Set.intersection r u

instance (Ord s) => Automata.Class.Acceptor NFA a s where
  accepts = accepts
