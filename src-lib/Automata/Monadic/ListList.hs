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
fromAFA afa = undefined

-- | TODO: implement & test
toAFA :: (Ord s) => ListListAutomaton a s -> AFA a s
toAFA m = undefined

-- | TODO: test
accepts :: (Ord s) => ListListAutomaton a s -> [a] -> Bool
accepts m w = acceptance $ acceptsM m w
  where
    acceptance = LL.asCNF id
