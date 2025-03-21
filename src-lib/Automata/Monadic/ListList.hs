-- | Monadic automaton with the `ListList` monad.
-- Equivalent to an alternating finite automaton,
-- where the nested list represents a positive CNF.
module Automata.Monadic.ListList where

import Automata.AFA (AFA (AFA))
import qualified Automata.AFA as AFA
import Automata.Monadic
import qualified Data.ListList as LL
import qualified Data.Set as Set
import Data.Universe.Class

type ListListAutomaton a s = AutomatonM a LL.ListList s

-- | TODO: test
fromAFA :: (Ord s, Finite s) => AFA a s -> ListListAutomaton a s
fromAFA afa = AutomatonM {start = _start, final = _final, trans = _trans}
  where
    _start = AFA.start afa
    _final = AFA.final afa
    _trans (q, a) =
      LL.fromSet $
        Set.filter
          (\rs -> AFA.trans afa q (a, rs))
          powerStates
    states = Set.fromList universeF
    powerStates = Set.powerSet states

-- | TODO: test
toAFA :: (Ord s) => ListListAutomaton a s -> AFA a s
toAFA m = AFA {AFA.start = _start, AFA.final = _final, AFA.trans = _trans}
  where
    _start = start m
    _final = final m
    _trans q (a, rs) = rs `Set.member` LL.toSet (trans m (q, a))

-- | TODO: test
accepts :: (Ord s) => ListListAutomaton a s -> [a] -> Bool
accepts m w = acceptance $ acceptsM m w
  where
    acceptance = LL.asCNF id
