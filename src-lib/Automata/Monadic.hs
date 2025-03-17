-- | Automata generalized with a mondaic transition function.
module Automata.Monadic where

import Control.Monad (foldM)
import Data.Set (Set)
import qualified Data.Set as Set

-- | A Monadic Automata is a generalization of finite state machines,
-- and is defined by an 8-tuple
--  ($M, Q, \Sigma, \delta_q, \delta_r, q_0, F$), where
--
--    1. $M$ is a monad,
--    2. $Q$ is a finite set called the *active states*,
--    4. $\Sigma$ is a finite set called the *alphabet*,
--    5. $\delta : Q \times \Sigma \rightarrow M(Q)$ is the *monadic transition function*,
--    7. $q_0 \in Q$ is the *start state*, and
--    8. $F \subseteq Q$ is the *set of final states*.
data AutomatonM a m s = AutomatonM
  { -- | The monadic transition function $\delta$.
    trans :: (s, a) -> m s,
    -- | The start state $q_0$.
    start :: s,
    -- | The set of final states $F$.
    final :: Set s
  }

stepM :: AutomatonM a m s -> s -> a -> m s
stepM m = curry (trans m)

accepts :: (Ord s, Monad m) => AutomatonM a m s -> [a] -> m Bool
accepts m xs = do
  let q_0 = start m
  q_n <- foldM (stepM m) q_0 xs
  pure $ q_n `Set.member` final m
