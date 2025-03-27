-- | Monadic automaton with the `Identity` monad.
-- Equivalent to a deterministic finite automaton.
module Automata.FiniteState.Monadic.Identity
  ( IdentityFA,
    accepts,
    fromDFA,
    toDFA,
  )
where

import Data.Functor.Identity
import Automata.FiniteState.DFA (DFA (DFA))
import qualified Automata.FiniteState.DFA as DFA
import Automata.FiniteState.Monadic

type IdentityFA a s = MonadicFA a Identity s

-- | TODO: Test that this works.
fromDFA :: DFA a s -> IdentityFA a s
fromDFA dfa = MonadicFA {start = _start, final = _final, trans = _trans}
  where
    _start = DFA.start dfa
    _final = DFA.final dfa
    _trans = pure . DFA.trans dfa

-- | TODO: Test that this works.
toDFA :: IdentityFA a s -> DFA a s
toDFA m = DFA {DFA.start = _start, DFA.final = _final, DFA.trans = _trans}
  where
    _start = start m
    _final = final m
    _trans = runIdentity . trans m

accepts :: (Ord s) => IdentityFA a s -> [a] -> Bool
accepts m w = acceptance $ runMFA m w
  where
    acceptance = runIdentity
