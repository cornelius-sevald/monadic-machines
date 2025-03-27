-- | Monadic automaton with the `List` monad.
-- Equivalent to a non-deterministic finite automaton.
module Automata.FiniteState.Monadic.List
  ( ListFA,
    fromNFA,
    toNFA,
    accepts,
  )
where

import Control.Applicative (Alternative (empty))
import qualified Data.Set as Set
import Data.Universe.Class (Finite)
import Automata.FiniteState.Monadic
import Automata.FiniteState.NFA (NFA (NFA))
import qualified Automata.FiniteState.NFA as NFA

type ListFA a s = MonadicFA a [] s

-- | TODO: Test that this works.
fromNFA :: (Ord s, Finite s) => NFA a s -> ListFA a s
fromNFA nfa = MonadicFA {start = _start, final = _final, trans = _trans}
  where
    _start = NFA.start nfa
    _final = NFA.prefinal nfa
    _trans = Set.toList . uncurry (NFA.step nfa)

-- | TODO: Test that this works.
toNFA :: ListFA a s -> NFA a s
toNFA m = NFA {NFA.start = _start, NFA.final = _final, NFA.trans = _trans}
  where
    _start = start m
    _final = final m
    _trans (q, Just x) = trans m (q, x)
    _trans (_, Nothing) = empty

accepts :: (Ord s) => ListFA a s -> [a] -> Bool
accepts m w = acceptance $ runMFA m w
  where
    acceptance = or
