-- | Finite-state automata generalized with a mondaic transition function.
module Automata.FiniteState.Monadic
  ( MonadicFA (..),
    stepMFA,
    runMFA,
    runMFA',
  )
where

import Control.Monad (foldM)
import Data.Set (Set)
import qualified Data.Set as Set

-- | A Monadic Finite Automaton is a generalization of finite state machines,
-- and is defined by an 8-tuple
--  ($M, Q, \Sigma, \delta_q, \delta_r, q_0, F$), where
--
--    1. $M$ is a monad,
--    2. $Q$ is a finite set called the *active states*,
--    4. $\Sigma$ is a finite set called the *alphabet*,
--    5. $\delta : Q \times \Sigma \rightarrow M(Q)$ is the *monadic transition function*,
--    7. $q_0 \in Q$ is the *start state*, and
--    8. $F \subseteq Q$ is the *set of final states*.
--
-- TODO: Update this description, it's outdated.
data MonadicFA a m s = MonadicFA
  { -- | The start state $q_0$.
    start :: s,
    -- | The set of final states $F$.
    final :: Set s,
    -- | The monadic transition function $\delta$.
    trans :: (s, a) -> m s
  }

stepMFA :: MonadicFA a m s -> s -> a -> m s
stepMFA m = curry (trans m)

-- | Run a Monadic Finite Automaton @m@ on input @xs@,
-- resultuing in a monadic Boolean value.
runMFA :: (Ord s, Monad m) => MonadicFA a m s -> [a] -> m Bool
runMFA m xs = do
  let q_0 = start m
  q_n <- foldM (stepMFA m) q_0 xs
  pure $ q_n `Set.member` final m

-- | Like 'runMFA', but allows supplying a "shrink" function
-- that will be applied between each step.
-- An example might be removing duplicate elements in a list,
-- as they have no impact on the semantics when using standard
-- angelic or demonic non-determinism,
-- but do impose a significant performance overhead.
runMFA' :: (Ord s, Monad m) => MonadicFA a m s -> (m s -> m s) -> [a] -> m Bool
runMFA' m shrink xs = do
  let q_0 = start m
  q_n <-
    let c x k z = shrink (stepMFA m z x) >>= (shrink . k)
     in foldr c return xs q_0
  pure $ q_n `Set.member` final m
