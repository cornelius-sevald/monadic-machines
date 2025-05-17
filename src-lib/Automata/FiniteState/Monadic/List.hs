-- | Monadic automaton with the `List` monad.
-- Equivalent to a non-deterministic finite automaton,
-- with angelic non-determinism acceptance.
module Automata.FiniteState.Monadic.List
  ( ListFA,
    fromNFA,
    toNFA,
    acceptsAngelic,
    acceptsDemonic,
  )
where

import Automata.FiniteState.Monadic
import Automata.FiniteState.NFA (NFA (NFA))
import qualified Automata.FiniteState.NFA as NFA
import Data.Containers.ListUtils (nubOrd)
import qualified Data.Set as Set
import Data.Universe.Class (Finite)

type ListFA a s = MonadicFA a [] s

fromNFA :: (Ord s, Finite s) => NFA a s -> ListFA a s
fromNFA nfa = MonadicFA {start = _start, final = _final, trans = _trans}
  where
    _start = NFA.start nfa
    _final = NFA.prefinal nfa
    _trans = Set.toList . uncurry (NFA.step nfa)

toNFA :: (Ord s) => ListFA a s -> NFA a s
toNFA m = NFA {NFA.start = _start, NFA.final = _final, NFA.trans = _trans}
  where
    _start = start m
    _final = final m
    _trans (q, Just x) = Set.fromList $ trans m (q, x)
    _trans (_, Nothing) = Set.empty

acceptsAngelic :: (Ord s) => ListFA a s -> [a] -> Bool
acceptsAngelic m w = acceptance $ runMFA' m removeDups w
  where
    acceptance = or
    removeDups = nubOrd

acceptsDemonic :: (Ord s) => ListFA a s -> [a] -> Bool
acceptsDemonic m w = acceptance $ runMFA' m removeDups w
  where
    acceptance = and
    removeDups = nubOrd
