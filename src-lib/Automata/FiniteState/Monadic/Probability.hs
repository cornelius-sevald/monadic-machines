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

type ProbabilityFA prob a s = MonadicFA a (T prob) s

-- | Acceptance function for PFA.
--
-- Takes a *cut-point* 0 ≤ η < 1 which is the threshold
-- at which words are accepted, i.e. a larger value
-- of η means that the automaton is less likely to accepts.
--
-- NOTE: If running the FA results in an "empty" probability,
-- this will throw an error. However, you should not make empty
-- transitions in the probability monad anyway, as it results
-- in invalid probabilities when running the FA, i.e. you may
-- get results where P(accept) + P(reject) < 1.
accepts ::
  (Ord s, Ord prob, Num prob) =>
  ProbabilityFA prob a s ->
  prob ->
  [a] ->
  Bool
accepts m η w = acceptance $ runMFA' m Dist.norm w
  where
    acceptance t = Dist.truth t > η
