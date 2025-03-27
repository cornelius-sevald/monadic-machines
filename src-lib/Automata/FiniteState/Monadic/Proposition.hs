-- | Monadic automaton with the `Proposition` monad.
-- Equivalent to an alternating finite automaton,
-- augmented with negation as well.
module Automata.FiniteState.Monadic.Proposition
  ( PropositionFA,
    fromAFA,
    toAFA,
    accepts,
  )
where

import Data.Logic.Proposition
import Automata.FiniteState.AFA (AFA (AFA))
import qualified Automata.FiniteState.AFA as AFA
import Automata.FiniteState.Monadic

type PropositionFA a s = MonadicFA a Proposition s

-- | TODO: Implement & test
fromAFA :: (Ord s) => AFA a s -> PropositionFA a s
fromAFA afa = error "not implemented"

-- | TODO: Implement & test
toAFA :: PropositionFA a s -> AFA a s
toAFA m = error "not implemented"

-- | TODO: test
accepts :: (Ord s) => PropositionFA a s -> [a] -> Bool
accepts m w = acceptance $ runMFA m w
  where
    acceptance = evaluate id
