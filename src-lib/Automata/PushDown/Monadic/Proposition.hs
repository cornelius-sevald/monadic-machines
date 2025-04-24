-- | Monadic pushdown automaton with the `Proposition` monad.
-- Equivalent to an alternating pushdown automaton,
-- augmented with negation as well.
module Automata.PushDown.Monadic.Proposition
  ( PropositionPDA,
    accepts,
  )
where

import Automata.PushDown.Monadic
import Data.Logic.Proposition

type PropositionPDA r p a t = MonadicPDA Proposition r p a t

accepts :: (Ord r) => PropositionPDA r p a t -> [a] -> Bool
accepts m w = acceptance $ runMPDA m w
  where
    acceptance = evaluate id
