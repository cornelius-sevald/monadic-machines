{-# LANGUAGE OverloadedLists #-}

-- | Pushdown automata generalized with a mondaic transition function.
module Automata.PushDown.Monadic where

import Control.Monad (foldM)
import Data.Set (Set)
import qualified Data.Set as Set

-- | A Monadic Pushdown Automaton (MPDA) is a generalization
-- of the Functional Pushdown Automaton, and is defined by a
-- 9-tuple (M, Q, Σ, Γ, δ, γ, Z_1, q_1, F) where,
--
--   1. M is a monad.
--   2. Q is a finite set called the *states*,
--   3. Σ is a finite set called the *input alphabet*,
--   4. Γ is a finite set called the *stack alphabet*,
--   5. δ : Q × Γ × Σ → M(Q × Γ^* × {0,1}) is the *input transition function*,
--   6. γ : Q × Γ → M(Q) is the *stack transition function*,
--   7. Z_1 ∈ Γ is the *start stack symbol*, and
--   8. q_1 ∈ Q is the *start state*, and
--   9. F ⊆ Q is the set of *accepting states*.
--
-- The notation Γ^* denotes the set {ε} ∪ Γ ∪ (Γ × Γ) ∪ ....
--
-- The monad, states, input- and stack alphabet is implicitly given by the types
-- `m`, `s`, `a` and `t` respectively.
data MonadicPDA m r p a t = MonadicPDA
  { startSymbol :: t,
    startState :: r,
    finalStates :: Set r,
    transRead :: (r, a) -> m (Either (r, [t]) p),
    transPop :: (p, t) -> m (Either (r, [t]) p)
  }

catch :: Either x p -> Maybe x
catch (Left x) = Just x
catch (Right _) = Nothing

stepPop :: (Monad m) => MonadicPDA m r p a t -> Either (r, [t]) p -> t -> m (Either (r, [t]) p)
stepPop _ (Left (r, ts)) t = pure $ Left (r, ts <> [t])
stepPop m (Right p) t = transPop m (p, t)

stepRead :: (Monad m) => MonadicPDA m r p a t -> (r, [t]) -> a -> m (Maybe (r, [t]))
stepRead m (r, ts) a = do
  let f = stepPop m
  s <- transRead m (r, a)
  catch <$> foldM f s ts

stepsM :: (Monad m) => MonadicPDA m r p a t -> [a] -> (r, [t]) -> m (Maybe (r, [t]))
stepsM m as c =
  let f c' a = case c' of
        Nothing -> pure Nothing
        Just (r, ts) -> stepRead m (r, ts) a
   in foldM f (pure c) as

runMPDA :: (Monad m, Ord r) => MonadicPDA m r p a t -> [a] -> m Bool
runMPDA m as = do
  let c_0 = (startState m, [startSymbol m])
  c_n <- stepsM m as c_0
  case c_n of
    Nothing -> pure False
    Just (q, _) -> pure $ q `Set.member` finalStates m
