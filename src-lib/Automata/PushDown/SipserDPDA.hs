-- | A Sipser Deterministic Pushdown Automata.
module Automata.PushDown.SipserDPDA where

import Data.List (find)
import Data.Maybe (catMaybes, listToMaybe, maybeToList)
import Data.Set (Set)
import qualified Data.Set as Set

-- | A Sipser Deterministic Pushdown Automaton (SipserDPDA)
-- is a 6-tuple (Q, Σ, Γ, δ, q_1, F), where
--
--    1. Q is a finite set called the *states*,
--    2. Σ is a finite set called the *input alphabet*,
--    2. Γ is a finite set called the *stack alphabet*,
--    3. δ : Q × Γ_ε × Σ_ε → (Q × Γ_ε) ∪ {∅} is the transition function,
--    4. q_1 ∈ Q is the *start state*, and
--    5. F ⊆ Q is the *set of final states*.
--
-- The transition function must satisfy the following condition that,
-- for every q ∈ Q, a ∈ Σ, and t ∈ Γ, exactly one of the values
--     δ(q, a, t), δ(q, a, ε), δ(q, ε, t), and δ(q, ε, ε)
-- is not ∅. [1]
--
-- Note that this definition only allows a paritally functional implementation,
-- i.e. one in which each step has *at most* one result, rather than
-- exactly one result. Take for example the case where we are in a state s
-- and the stack and input are empty, but δ(s, ε, ε) = ∅.
--
-- The states, input- and stack alphabet is implicitly given by the types
-- `s`, `a` and `t` respectively.
data SipserDPDA s a t = SipserDPDA
  { -- | The start state q_1.
    start :: s,
    -- | The set of final states F.
    final :: Set s,
    -- | The transition function δ.
    trans :: (s, Maybe t, Maybe a) -> Maybe (s, Maybe t)
  }

-- | Check if we have been in a similar configuration before.
-- Specifically, check if we have been in a configuration
-- with the same state, same top of the stack, and not a smaller stack.
--
-- This is equivalent to the `existsIn` function from [2].
dejavu :: (Eq s, Eq t) => [(s, [t])] -> (s, [t]) -> Bool
dejavu before now = any (been now) before
  where
    been (s, ts) (q, ys) =
      -- States match,
      s == q
        -- and the top of the stacks match,
        && listToMaybe ts == listToMaybe ys
        -- and the `now` stack has not shrunk.
        && length ts >= length ys

-- | Return the first state/stack pair (s, t) for which s is final, if any.
-- Otherwise, return `Nothing`.
--
-- This is similar to the `reachable` function from [2].
firstFinal :: (Ord s) => SipserDPDA s a t -> [(s, [t])] -> Maybe (s, [t])
firstFinal dpda = find (\(s, _) -> s `Set.member` final dpda)

-- | Perform a step in state `s` with (optional) input symbol `a`.
--
-- The step can either pop the stack or leave it be, but may not do both,
-- or it may be undefined for the given (or not given) input symbol.
-- We try both, and choose whichever one was successful (if any),
-- and return `Just` the successor state, along with the remaining stack.
-- If none of the two options were successful, we return `Nothing`,
stepStack :: SipserDPDA s a t -> s -> [t] -> Maybe a -> Maybe (s, [t])
stepStack dpda s ts a =
  let ress = catMaybes $ case ts of
        [] -> [go Nothing []]
        (t : ts') -> [go Nothing ts, go (Just t) ts']
   in case ress of
        -- both δ(s, ε, a) = ∅ and δ(s, t, a) = ∅.
        [] -> Nothing
        -- exactly one of δ(s, ε, a) and δ(s, t, a) are not ∅.
        [res] -> Just res
        -- neither δ(s, ε, a) = ∅ nor δ(s, t, a) = ∅.
        -- This is not allowed, as it would introduce non-determinism.
        [_, _] ->
          let x_str = case a of Nothing -> "ε"; _ -> "a"
              msg =
                "Automata.PushDown.SipserDPDA.stepStack: "
                  ++ "transition function is defined for both "
                  ++ ("(s, ε, " ++ x_str ++ ") and (s, t, " ++ x_str ++ ").")
           in error msg
        -- `ress` clearly has length <= 2, so this should never happen.
        _ -> error "Automata.PushDown.SipserDPDA.stepStack: `length ress` > 2 (this should never happen)"
  where
    go t ts' = do
      (s', t') <- trans dpda (s, t, a)
      pure (s', maybeToList t' ++ ts')

-- | Perform a step without consuming any input.
-- Either this results in a new configuration (state/stack pair),
-- or we reach an infinite loop of states.
stepE ::
  (Eq s, Eq t) =>
  -- | The DPDA.
  SipserDPDA s a t ->
  -- | A list of state/stack configurations we have previously seen.
  [(s, [t])] ->
  -- | The current state/stack configuration.
  (s, [t]) ->
  -- | Either the new state/stack configuration, or the states of the infinte loop.
  Either (s, [t]) [s]
stepE dpda seen c@(s, ts) =
  -- Step the stack, trying both popping the top symbol and leaving it be.
  case stepStack dpda s ts Nothing of
    -- δ(s, ε, ε) = ∅ and either the stack is empty, or δ(s, t, ε) = ∅.
    Nothing -> Left c
    -- exactly one of δ(s, ε, ε) and δ(s, t, ε) are not ∅.
    Just c' ->
      let seen' = c : seen
       in if dejavu seen' c'
            -- If we have been in a similar configuration,
            -- we are in an infinite loop.
            then Right $ fst <$> seen'
            -- Otherwise we step on this new configuration.
            else stepE dpda seen' c'

step :: (Eq s, Eq t) => SipserDPDA s a t -> a -> (s, [t]) -> Either (s, [t]) [s]
step dpda a (s, ts) =
  case stepE dpda [] (s, ts) of
    -- We have reached an infinite loop of states `ss`.
    Right ss -> Right ss
    -- There are no more ε-transition available,
    -- so we know that δ(s, ε, ε) = ∅ and either the stack is empty, or δ(s, t, ε) = ∅.
    Left (s', ts') ->
      case stepStack dpda s' ts' (Just a) of
        -- δ(s, ε, a) = ∅ and either the stack is empty, or δ(s, t, a) = ∅.
        -- This gives us two cases:
        --   1. if the stack not empty, then neither
        --      δ(s, ε, ε), δ(s, ε, a), δ(s, t, ε) nor δ(s, t, a) is defined,
        --      which is an error.
        --   2. if the stack is empty, one of δ(s, t, ε) or δ(s, t, a) might be defined,
        --      and if it is, we reject the input as we can't pop from an empty stack. [1, p. 131]
        --      They might also not be defined, which would be an error,
        --      but I can't justify "forging" an arbitrary value for `t`
        --      just to check, and we are already working under the assumption
        --      that exactly one transition is defined,
        --      so we just reject the input even if there might be an error.
        --
        --  To actually signal a rejection, we return `Right []`,
        --  i.e. an infinite loop of zero states, which doesn't make much
        --  sense but is a nice way to say that the string should be rejected.
        Nothing ->
          if null ts'
            then Right []
            else
              let msg =
                    "Automata.PushDown.SipserDPDA.step: "
                      ++ "transition function is not defined for any of "
                      ++ "(s, ε, ε), (s, ε, a), (s, t, ε) or (s, t, a)."
               in error msg
        -- exactly one of δ(s, ε, a) and δ(s, t, a) are not ∅.
        Just c' -> Left c'

accepts :: (Ord s, Eq t) => SipserDPDA s a t -> [a] -> Bool
accepts dpda as = go as (start dpda, [])
  where
    go [] c =
      case stepE dpda [] c of
        -- When we have read all input, check if the current state is final.
        Left (s', _) -> s' `Set.member` final dpda
        -- If we have read all input and is in an infinite cycle,
        -- we accept if any of the reachable states are final.
        Right ss -> any (`Set.member` final dpda) ss
    go (a : as') c =
      case step dpda a c of
        Left c' -> go as' c'
        -- If we are in an infinite cycle but have not read all input,
        -- we reject the string. [1, p. 131]
        Right _ -> False

{- Bibliography
 - ~~~~~~~~~~~~
 - [1]: M. Sipser, Introduction to the theory of computation, Third edition. Cengage Learning, 2013.
 - [2]: T. D. Randløv, “Toward a Monadic Functional Machine Model for Computability and Complexity Theory: Finite and Pushdown Automata,” Master’s Thesis, University of Copenhagen, 2023.
-}
