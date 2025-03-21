-- | Monadic automaton with the `Proposition` monad.
-- Equivalent to an alternating finite automaton,
-- augmented with negation as well.
module Automata.Monadic.Proposition where

import Automata.AFA (AFA (AFA))
import qualified Automata.AFA as AFA
import Automata.Monadic
import Data.Proposition

type PropositionAutomaton a s = AutomatonM a Proposition s

-- | TODO: Implement & test
fromAFA :: (Ord s) => AFA a s -> PropositionAutomaton a s
fromAFA afa = error "not implemented"

-- | TODO: Implement & test
toAFA :: PropositionAutomaton a s -> AFA a s
toAFA m = error "not implemented"

-- | TODO: test
accepts :: (Ord s) => PropositionAutomaton a s -> [a] -> Bool
accepts m w = acceptance $ acceptsM m w
  where
    acceptance = evaluate id
