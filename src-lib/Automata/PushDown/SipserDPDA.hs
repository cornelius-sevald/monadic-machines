-- | A Sipser Deterministic Pushdown Automata.
module Automata.PushDown.SipserDPDA where

import Automata.PushDown.Util
import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe (mapMaybe, maybeToList)
import Data.Set (Set)
import qualified Data.Set as Set
import GHC.IsList

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
    trans :: (s, Maybe t, Maybe a) -> Maybe (s, [t])
  }

-- | Perform a step in state `s` with (optional) input symbol `a`.
--
-- The step can either pop the stack or leave it be, but may not do both,
-- or it may be undefined for the given (or not given) input symbol.
-- We try both, and choose whichever one was successful (if any),
-- and return `Just` the successor state, along with the remaining stack.
-- If none of the two options were successful, we return `Nothing`,
stepStack :: SipserDPDA s a t -> s -> [t] -> Maybe a -> Maybe (s, [t])
stepStack pda s ts a =
  let ress = mapMaybe go $ split01 ts
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
    go (t, ts') = do
      (s', t') <- trans pda (s, t, a)
      pure (s', t' ++ ts')

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
  -- | Either a list of all state/stack configuration encountered until halting,
  -- or the states encountered until an infinite loop was reached.
  Either (NonEmpty (s, [t])) (NonEmpty s)
stepE pda seen c@(s, ts) =
  -- Step the stack, trying both popping the top symbol and leaving it be.
  case stepStack pda s ts Nothing of
    -- δ(s, ε, ε) = ∅ and either the stack is empty, or δ(s, t, ε) = ∅.
    Nothing -> Left $ c :| seen
    -- exactly one of δ(s, ε, ε) and δ(s, t, ε) are not ∅.
    Just c' ->
      let seen' = c : seen
       in case dejavu seen' c' of
            -- If we have been in a similar configuration,
            -- we are in an infinite loop.
            Just (s', _) -> Right $ s' :| (fst <$> seen')
            -- Otherwise we step on this new configuration.
            Nothing -> stepE pda seen' c'

step :: (Eq s, Eq t) => SipserDPDA s a t -> a -> (s, [t]) -> Either (s, [t]) [s]
step pda a (s, ts) =
  case stepE pda [] (s, ts) of
    -- We have reached an infinite loop of states `ss`.
    Right ss -> Right $ toList ss
    -- There are no more ε-transition available,
    -- so we know that δ(s, ε, ε) = ∅ and either the stack is empty, or δ(s, t, ε) = ∅.
    Left ((s', ts') :| _) ->
      case stepStack pda s' ts' (Just a) of
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
accepts pda as = go as (start pda, [])
  where
    go [] c =
      case stepE pda [] c of
        -- When we have read all input, check if the current state is final.
        Left cs ->
          let ss = fst <$> cs
           in any (`Set.member` final pda) ss
        -- If we have read all input and is in an infinite cycle,
        -- we accept if any of the reachable states are final.
        Right ss -> any (`Set.member` final pda) ss
    go (a : as') c =
      case step pda a c of
        Left c' -> go as' c'
        -- If we are in an infinite cycle but have not read all input,
        -- we reject the string. [1, p. 131]
        Right _ -> False

{- Bibliography
 - ~~~~~~~~~~~~
 - [1]: M. Sipser, Introduction to the theory of computation, Third edition. Cengage Learning, 2013.
 - [2]: T. D. Randløv, “Toward a Monadic Functional Machine Model for Computability and Complexity Theory: Finite and Pushdown Automata,” Master’s Thesis, University of Copenhagen, 2023.
-}
