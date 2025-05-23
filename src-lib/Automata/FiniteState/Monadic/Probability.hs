-- | Monadic automaton with the `Probability` monad.
-- Equivalent to a probabilistic finite automaton.
module Automata.FiniteState.Monadic.Probability
  ( ProbabilityFA,
    fromPFA,
    toPFA,
    acceptsStrict,
    acceptsNonStrict,
    invert,
  )
where

import Automata.FiniteState.Monadic
import Automata.FiniteState.PFA (PFA (PFA))
import qualified Automata.FiniteState.PFA as PFA
import qualified Data.Set as Set
import Data.Universe.Class (Finite (..))
import Numeric.Probability.Distribution (T)
import qualified Numeric.Probability.Distribution as Dist

type ProbabilityFA prob a s = MonadicFA a (T prob) s

fromPFA :: PFA prob a s -> ProbabilityFA prob a s
fromPFA pfa = MonadicFA {start = _start, final = _final, trans = _trans}
  where
    _start = PFA.start pfa
    _final = PFA.final pfa
    _trans = PFA.trans pfa

toPFA :: ProbabilityFA prob a s -> PFA prob a s
toPFA m = PFA {PFA.start = _start, PFA.final = _final, PFA.trans = _trans}
  where
    _start = start m
    _final = final m
    _trans = trans m

-- | Acceptance function for PFA, with strict comparison.
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
acceptsStrict ::
  (Ord s, Ord prob, Num prob) =>
  ProbabilityFA prob a s ->
  prob ->
  [a] ->
  Bool
acceptsStrict m η w = acceptance $ runMFA m w
  where
    acceptance t = Dist.truth t > η

-- | Alternative acceptance function for PFA, using non-strict comparison.
--
-- Takes a *cut-point* 0 < η ≤ 1 which is the threshold
-- at which words are accepted, i.e. a larger value
-- of η means that the automaton is less likely to accepts.
acceptsNonStrict ::
  (Ord s, Ord prob, Num prob) =>
  ProbabilityFA prob a s ->
  prob ->
  [a] ->
  Bool
acceptsNonStrict m η w = acceptance $ runMFA m w
  where
    acceptance t = Dist.truth t >= η

-- | Invert PFA M, such that `invert M` accepts a word `w`
-- with non-strict cut-point η iff. `M` rejects `w` with
-- strict cut-point (1-η), or vice versa.
invert ::
  (Ord s, Finite s, Fractional prob) =>
  ProbabilityFA prob a s ->
  ProbabilityFA prob a s
invert m =
  m {final = complement $ final m}
  where
    complement = (Set.fromList universeF `Set.difference`)
