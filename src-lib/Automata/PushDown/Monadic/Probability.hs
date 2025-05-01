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
accepts ::
  (Ord r, Ord prob, Num prob) =>
  ProbabilityPDA prob r p a t ->
  prob ->
  [a] ->
  Bool
accepts m η w = acceptance $ runMPDA m w
  where
    acceptance t =
      case Dist.decons t of
        [] -> False
        _ -> Dist.truth t > η
