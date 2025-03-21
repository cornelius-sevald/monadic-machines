-- | Monadic automaton with the `Identity` monad.
-- Equivalent to a deterministic finite automaton.
module Automata.Monadic.Identity (IdentityAutomaton, accepts, fromDFA, toDFA) where

import Automata.DFA (DFA (DFA))
import qualified Automata.DFA as DFA
import Automata.Monadic
import Data.Functor.Identity

type IdentityAutomaton a s = AutomatonM a Identity s

-- | TODO: Test that this works.
fromDFA :: DFA a s -> IdentityAutomaton a s
fromDFA dfa = AutomatonM {start = _start, final = _final, trans = _trans}
  where
    _start = DFA.start dfa
    _final = DFA.final dfa
    _trans = pure . DFA.trans dfa

-- | TODO: Test that this works.
toDFA :: IdentityAutomaton a s -> DFA a s
toDFA m = DFA {DFA.start = _start, DFA.final = _final, DFA.trans = _trans}
  where
    _start = start m
    _final = final m
    _trans = runIdentity . trans m

accepts :: (Ord s) => IdentityAutomaton a s -> [a] -> Bool
accepts m w = acceptance $ acceptsM m w
  where
    acceptance = runIdentity
