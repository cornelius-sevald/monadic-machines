{-# LANGUAGE NoImplicitPrelude #-}

-- | Finite-state automata generalized with a mondaic transition function.
module Automata.FiniteState.Monadic
  ( MonadicFA (..),
    runMFA,
    runMFA',
  )
where

import Control.Arrow
import Control.Category
import Data.Foldable
import Data.Set (Set)
import qualified Data.Set as Set
import Prelude hiding (id, (.))

-- | A Monadic Finite Automaton is a generalization of finite state machines,
-- and is defined by an 7-tuple (M, Q, Σ, δ, q_0, F), where
--
--    1. M is a monad,
--    2. Q is a finite set called the *states*,
--    3. Σ is a finite set called the *alphabet*,
--    4. δ : Q × Σ → M(Q) is the *monadic transition function*,
--    6. q_0 ∈ Q is the *start state*, and
--    7. F ⊆ Q is the *set of final states*.
data MonadicFA a m s = MonadicFA
  { -- | The start state $q_0$.
    start :: s,
    -- | The set of final states $F$.
    final :: Set s,
    -- | The monadic transition function $\delta$.
    trans :: (s, a) -> m s
  }

-- | Run a Monadic Finite Automaton @m@ on input @xs@,
-- resultuing in a monadic Boolean value.
runMFA :: (Ord s, Monad m) => MonadicFA a m s -> [a] -> m Bool
runMFA m xs = do
  let f x = Kleisli (\q -> trans m (q, x))
  let q_0 = start m
  q_n <- runKleisli (foldl' (>>>) id $ map f xs) q_0
  pure $ q_n `Set.member` final m

-- | Like 'runMFA', but allows supplying a "shrink" function
-- that will be applied between each step.
-- An example might be removing duplicate elements in a list,
-- as they have no impact on the semantics when using standard
-- angelic or demonic non-determinism,
-- but do impose a significant performance overhead.
runMFA' :: (Ord s, Monad m) => MonadicFA a m s -> (m s -> m s) -> [a] -> m Bool
runMFA' m shrink xs = do
  let f x = Kleisli (\q -> shrink $ trans m (q, x))
  let q_0 = start m
  q_n <- runKleisli (foldl' (>>>) id $ map f xs) q_0
  pure $ q_n `Set.member` final m
