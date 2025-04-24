-- | Monadic automaton with the `Probability` monad.
-- Equivalent to a probabilistic finite automaton.
module Automata.FiniteState.Monadic.Probability
  ( ProbabilityFA,
    accepts,
  )
where

import Automata.FiniteState.Monadic
import Numeric.Probability.Distribution (T)
import qualified Numeric.Probability.Distribution as Dist

type ProbabilityFA a s prob = MonadicFA a (T prob) s

-- | Acceptance function for PFA.
--
-- Takes a *cut-point* 0 ≤ η < 1 which is the threshold
-- at which words are accepted, i.e. a larger value
-- of η means that the automaton is less likely to accepts.
accepts ::
  (Ord s, Ord prob, Num prob) =>
  ProbabilityFA a s prob ->
  prob ->
  [a] ->
  Bool
accepts m η w = acceptance $ runMFA' m Dist.norm w
  where
    acceptance t =
      case Dist.decons t of
        [] -> False
        _ -> Dist.truth t > η
