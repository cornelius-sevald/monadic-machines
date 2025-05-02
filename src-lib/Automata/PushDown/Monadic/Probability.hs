-- | Monadic pushdown automaton with the `Probability` monad.
-- Equivalent to an probabilistic pushdown automaton.
module Automata.PushDown.Monadic.Probability
  ( ProbabilityPDA,
    accepts,
  )
where

import Automata.PushDown.Monadic
import Numeric.Probability.Distribution (T)
import qualified Numeric.Probability.Distribution as Dist

type ProbabilityPDA prob r p a t = MonadicPDA (T prob) r p a t

-- | Acceptance function for PPDA.
--
-- Takes a *cut-point* 0 ≤ η < 1 which is the threshold
-- at which words are accepted, i.e. a larger value
-- of η means that the automaton is less likely to accepts.
--
-- NOTE: If running the PDA results in an "empty" probability,
-- this will throw an error. However, you should not make empty
-- transitions in the probability monad anyway, as it results
-- in invalid probabilities when running the PDA, i.e. you may
-- get results where P(accept) + P(reject) < 1.
accepts ::
  (Ord r, Ord prob, Num prob) =>
  ProbabilityPDA prob r p a t ->
  prob ->
  [a] ->
  Bool
accepts m η w = acceptance $ runMPDA m w
  where
    acceptance t = Dist.truth t > η
