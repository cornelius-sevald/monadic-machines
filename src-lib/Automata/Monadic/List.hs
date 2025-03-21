-- | Monadic automaton with the `List` monad.
-- Equivalent to a non-deterministic finite automaton.
module Automata.Monadic.List (ListAutomaton, fromNFA, toNFA, accepts) where

import Automata.Monadic
import Automata.NFA (NFA (NFA))
import qualified Automata.NFA as NFA
import Control.Applicative (Alternative (empty))
import qualified Data.Set as Set
import Data.Universe.Class (Finite)

type ListAutomaton a s = AutomatonM a [] s

-- | TODO: Test that this works.
fromNFA :: (Ord s, Finite s) => NFA a s -> ListAutomaton a s
fromNFA nfa = AutomatonM {start = _start, final = _final, trans = _trans}
  where
    _start = NFA.start nfa
    _final = NFA.prefinal nfa
    _trans = Set.toList . uncurry (NFA.step nfa)

-- | TODO: Test that this works.
toNFA :: ListAutomaton a s -> NFA a s
toNFA m = NFA {NFA.start = _start, NFA.final = _final, NFA.trans = _trans}
  where
    _start = start m
    _final = final m
    _trans (q, Just x) = trans m (q, x)
    _trans (_, Nothing) = empty

accepts :: (Ord s) => ListAutomaton a s -> [a] -> Bool
accepts m w = acceptance $ acceptsM m w
  where
    acceptance = or
