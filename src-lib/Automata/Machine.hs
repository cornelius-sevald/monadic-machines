-- We need a better name for this module / type,
-- but we stick with "Machine" for now...

module Automata.Machine where

import Control.Monad (foldM)
import Data.Set (Set)
import qualified Data.Set as Set

-- | A Machine is a generalization of finite state machines,
-- and is defined by an 8-tuple
--  ($M, Q, R, \Sigma, \delta_q, \delta_r, r_0, F$), where
--
--    1. $M$ is a monad,
--    2. $Q$ is a finite set called the *active states*,
--    3. $R$ is a finite set called the *resting states*,
--    4. $\Sigma$ is a finite set called the *alphabet*,
--    5. $\delta_q : Q \times \Sigma \rightarrow R$ is the *active transition function*,
--    6. $\delta_r : R \rightarrow M(Q)$ is the *resting transition function*,
--    7. $r_0 \in R$ is the *start state*, and
--    8. $F \subseteq S$ is the *set of final states*.
data Machine a m s r = Machine
  { -- | The active transition function $\delta_q$.
    transA :: (s, a) -> r,
    -- | The resting transition function $\delta_r$.
    transR :: r -> m s,
    -- | The start state $r_0$.
    start :: r,
    -- | The set of final states $F$.
    final :: Set s
  }

-- | Step the machine in a resting state.
stepR :: Machine a m s r -> r -> m s
stepR = transR

-- | Step the machine in an active state @q@ with input symbol @@
stepA :: Machine a m s r -> s -> a -> r
stepA = curry . transA

step :: Machine a m s r -> s -> a -> m s
step m q x = stepR m $ stepA m q x

accepts :: (Ord s, Monad m) => Machine a m s r -> [a] -> m Bool
accepts m xs = do
  q_0 <- stepR m $ start m
  q_n <- foldM (step m) q_0 xs
  pure $ q_n `Set.member` final m
