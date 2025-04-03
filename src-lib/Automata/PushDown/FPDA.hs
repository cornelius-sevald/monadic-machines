{-# LANGUAGE TupleSections #-}

-- | 2-stack Deterministic Pushdown Automata.
module Automata.PushDown.FPDA where

import Automata.PushDown.SipserDPDA (SipserDPDA (SipserDPDA))
import qualified Automata.PushDown.SipserDPDA as SDPDA
import Automata.PushDown.Util
import Control.Arrow ((***))
import Data.Heart
import Data.Maybe (catMaybes, isJust, mapMaybe, maybeToList)
import Data.Set (Set)
import qualified Data.Set as Set

-- | A Functional Pushdown Automaton (FPDA),
-- is a 6-tuple (Q, Σ, Γ, δ, q_1, F) where,
--
--   1. Q is a finite set called the *states*,
--   2. Σ is a finite set called the *input alphabet*,
--   3. Γ is a finite set called the *stack alphabet*,
--   4. δ : Q × Γ_ε × Σ_ε → Q × Γ<3 × {0,1} is the *transition function*,
--   5. q_1 ∈ Q is the *start state*, and
--   6. F ⊆ Q is the set of *accepting states*.
--
-- Modified from the definition in [1].
--
-- The notation Γ<3 denotes the set {ε} ∪ Γ ∪ (Γ × Γ).
--
-- The boolean value returned by the transition function indicates
-- whether or not the input symbol is consumed or not,
-- i.e. if we pop the symbol from the input list or merely peek.
--
-- The states, input- and stack alphabet is implicitly given by the types
-- `s`, `a` and `t` respectively.
data FPDA s a t = FPDA
  { start :: s,
    final :: Set s,
    trans :: (s, Maybe t, Maybe a) -> (s, Heart t, Bool)
  }

-- | Step the FPDA from one configuration to the next.
--
-- The implementation is based on the `transitionFPDA` from [1].
step :: FPDA s a t -> (s, [t], [a]) -> (s, [t], [a])
step dpda (s, ts, as) =
  let (a', as') = split1 as
      (t', ts') = split1 ts
      (s', us, consume) = trans dpda (s, t', a')
      ts'' = heartToList us ++ ts'
      as'' = if consume then as' else as
   in (s', ts'', as'')

-- | Transitively step the FPDA until an infinite loop is found.
--
-- As each state has exactly one successor,
-- we will always end up in an infinite loop.
-- We then return the list of states that were reached
-- since last reading a new input symbol, as well as the remaining input
--
-- The implementation is based on the `stepsFPDA` from [1],
-- but I found that it can loop forever by bouncing between
-- two different states when there is no input left.
steps ::
  (Eq s, Eq a, Eq t) =>
  FPDA s a t ->
  [(s, [t])] ->
  (s, [t], [a]) ->
  ([s], [a])
steps dpda seen c@(s, t, a) =
  let c'@(s', t', a') = step dpda c
      -- If we have read an input symbol, we clear the `seen` list,
      -- as we are then in a new configuration.
      -- Otherwise, we add the last state/stack pair to the previously seen ones.
      seen' = if length a' < length a then [] else (s, t) : seen
   in if dejavu seen' (s', t')
        then (fst <$> seen', a')
        else steps dpda seen' c'

accepts :: (Ord s, Eq a, Eq t) => FPDA s a t -> [a] -> Bool
accepts dpda as =
  let (ss, a') = steps dpda [] (start dpda, [], as)
   in -- We accept if we have read all input,
      -- and any of the reachable states from there is a final one.
      null a' && any (`Set.member` final dpda) ss

fromSipserDPDA :: SipserDPDA s a t -> FPDA s a t
fromSipserDPDA pda =
  FPDA
    { start = SDPDA.start pda,
      final = SDPDA.final pda,
      trans = δ'
    }
  where
    splitMaybe x = case x of Nothing -> [Nothing]; Just y -> [Nothing, Just y]
    δ = SDPDA.trans pda
    δ' (s, t, a) =
      let input = (,) <$> splitMaybe t <*> splitMaybe a
          cs = (\(t', a') -> (s, t', a')) <$> input
          cs' = do
            (c, i) <- zip cs input
            Just c' <- pure $ δ c
            pure (c', i)
       in case cs' of
            -- Case 0: δ(s, ε, ε) = ∅ and either stack and input is empty,
            -- only stack is empty and δ(s, ε, a) = ∅, or,
            -- only input is empty and δ(s, t, ε) = ∅.
            -- In any case, we stay in the same configuration.
            [] -> (s, maybeToHeart t, False)
            -- Case 1: δ(s, ε, ε) ≠ ∅.
            -- We push @t@ and @t'@ to the stack, and consume no input.
            [((s', t'), (Nothing, Nothing))] -> (s', maybesToHeart t' t, False)
            -- Case 2: δ(s, t, ε) ≠ ∅.
            -- We push @t'@ to the stack (implicitly popping @t@), and consume no input.
            [((s', t'), (Just _, Nothing))] -> (s', maybeToHeart t', False)
            -- Case 3: δ(s, ε, a) ≠ ∅.
            -- We push @t@ and @t'@ to the stack, and consume the input.
            [((s', t'), (Nothing, Just _))] -> (s', maybesToHeart t' t, True)
            -- Case 4: δ(s, t, a) ≠ ∅.
            -- We push @t'@ to the stack (implicitly popping @t@), and consume the input.
            [((s', t'), (Just _, Just _))] -> (s', maybeToHeart t', True)
            -- Case 5: More than one of δ(s, ε, ε), δ(s, t, ε), δ(s, ε, a), δ(s, t, a)
            -- is defined, which is an error.
            -- We construct a nice error message showing for which input δ is defined.
            _ ->
              let ε_str x = maybe "ε" (const x)
                  δ_str (t', a') = "δ(s, " ++ ε_str "t" t' ++ ", " ++ ε_str "a" a' ++ ")"
                  msg =
                    "Automata.PushDown.FPDA.fromSipserDPDA: "
                      ++ "transition function of Sipser DPDA is defined for "
                      ++ show (δ_str . snd <$> cs')
                      ++ " but may only be defined for one."
               in error msg

toSipserDPDA :: FPDA s a t -> SipserDPDA s a t
toSipserDPDA pda = undefined

{- Bibliography
 - ~~~~~~~~~~~~
 - [1]: T. D. Randløv, “Toward a Monadic Functional Machine Model for Computability and Complexity Theory: Finite and Pushdown Automata,” Master’s Thesis, University of Copenhagen, 2023.
-}
