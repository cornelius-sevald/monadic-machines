-- | Monadic finite-state automaton with the `Identity` monad.
-- Equivalent to a deterministic finite automaton.
module Automata.FiniteState.Monadic.Identity
  ( IdentityFA,
    accepts,
    fromDFA,
    toDFA,
  )
where

import Automata.FiniteState.DFA (DFA (DFA))
import qualified Automata.FiniteState.DFA as DFA
import Automata.FiniteState.Monadic
import Data.Functor.Identity

type IdentityFA a s = MonadicFA a Identity s

accepts :: (Ord s) => IdentityFA a s -> [a] -> Bool
accepts m w = acceptance $ runMFA m w
  where
    acceptance = runIdentity

fromDFA :: DFA a s -> IdentityFA a s
fromDFA dfa =
  MonadicFA
    { start = DFA.start dfa,
      final = DFA.final dfa,
      trans = Identity . DFA.trans dfa
    }

toDFA :: IdentityFA a s -> DFA a s
toDFA m =
  DFA
    { DFA.start = start m,
      DFA.final = final m,
      DFA.trans = runIdentity . trans m
    }
