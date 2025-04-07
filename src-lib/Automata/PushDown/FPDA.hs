{-# LANGUAGE TupleSections #-}

-- | Functional Determinstic Pushdown Automata
module Automata.PushDown.FPDA where

import Automata.PushDown.SipserDPDA (SipserDPDA (SipserDPDA))
import qualified Automata.PushDown.SipserDPDA as SDPDA
import Automata.PushDown.Util
import Data.Foldable (find)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.Maybe (fromMaybe, maybeToList)
import Data.Set (Set)
import qualified Data.Set as Set

-- | A Functional Pushdown Automaton (FPDA),
-- is a 8-tuple (Q, Σ, Γ, δ, γ, Z_1, q_1, F) where,
--
--   1. Q is a finite set called the *states*,
--   2. Σ is a finite set called the *input alphabet*,
--   3. Γ is a finite set called the *stack alphabet*,
--   4. δ : Q × Γ × Σ → Q × Γ^* × {0,1} is the *input transition function*,
--   5. γ : Q × Γ → Q is the *stack transition function*,
--   6. Z_1 ∈ Γ is the *start stack symbol*, and
--   7. q_1 ∈ Q is the *start state*, and
--   8. F ⊆ Q is the set of *accepting states*.
--
-- The notation Γ^* denotes the set {ε} ∪ Γ ∪ (Γ × Γ) ∪ ....
--
-- The states, input- and stack alphabet is implicitly given by the types
-- `s`, `a`, `t` respectively.
--
-- TODO: Maybe ensure that the amount read from the stack
-- in the transition function is statically guaranteed to be ≤ n,
-- perheaps via a refinement type.
data FPDA s a t = FPDA
  { start :: s,
    final :: Set s,
    startSymbol :: t,
    transInput :: (s, t, a) -> (s, [t], Bool),
    transStack :: (s, t) -> s
  }

-- | Step the FPDA from one configuration to the next,
-- with input symbol @a@.
stepInput :: FPDA s a t -> (s, NonEmpty t, NonEmpty a) -> (s, [t], [a])
stepInput dpda (s, t :| ts, a :| as) =
  let (s', ts', consume) = transInput dpda (s, t, a)
      as' = if consume then as else a : as
   in (s', ts' <> ts, as')

stepsInput :: (Eq s, Eq a, Eq t) => FPDA s a t -> [(s, [t])] -> (s, [t], [a]) -> Maybe (s, [t])
stepsInput _ _ (s, [], _) = Just (s, [])
stepsInput _ _ (s, ts, []) = Just (s, ts)
stepsInput dpda seen (s, t : ts, a : as) =
  case dejavu seen (s, t : ts) of
    Just _ -> Nothing
    Nothing ->
      let (s', ts', as') = stepInput dpda (s, t :| ts, a :| as)
          seen' = if as' == (a : as) then (s, t : ts) : seen else []
       in stepsInput dpda seen' (s', ts', as')

stepStack :: FPDA s a t -> s -> t -> s
stepStack dpda s t = transStack dpda (s, t)

stepsStack :: FPDA s a t -> s -> [t] -> s
stepsStack dpda = foldl (stepStack dpda)

accepts :: (Ord s, Eq a, Eq t) => FPDA s a t -> [a] -> Bool
accepts dpda as =
  case stepsInput dpda [] (start dpda, [startSymbol dpda], as) of
    Nothing -> False
    Just (s', ts') ->
      let s'' = stepsStack dpda s' ts'
       in s'' `Set.member` final dpda

fromSipserDPDA :: (Ord s, Eq t) => SipserDPDA s a t -> FPDA s a (Maybe t)
fromSipserDPDA pda =
  FPDA
    { start = SDPDA.start pda,
      startSymbol = Nothing,
      final = _final,
      transInput = _transInput,
      transStack = _transStack
    }
  where
    splitMaybe x = case x of Nothing -> [Nothing]; Just y -> [Nothing, Just y]
    _final = SDPDA.final pda
    δ = SDPDA.trans pda
    _transInput (s, t, a) =
      let input = (,) <$> splitMaybe t <*> splitMaybe (Just a)
          cs = (\(t', a') -> (s, t', a')) <$> input
          cs' = do
            (c, i) <- zip cs input
            Just c' <- pure $ δ c
            pure (c', i)
       in case cs' of
            -- Case 0: δ(s, ε, ε) = ∅ and stack is empty and δ(s, ε, a) = ∅.
            -- We stay in the same configuration, leading to a loop.
            [] -> (s, [t], False)
            -- Case 1: δ(s, ε, ε) ≠ ∅.
            -- We push @t@ and @t'@ to the stack, and consume no input.
            [((s', t'), (Nothing, Nothing))] -> (s', (Just <$> maybeToList t') ++ [t], False)
            -- Case 2: δ(s, t, ε) ≠ ∅.
            -- We push @t'@ to the stack (implicitly popping @t@), and consume no input.
            [((s', t'), (Just _, Nothing))] -> (s', Just <$> maybeToList t', False)
            -- Case 3: δ(s, ε, a) ≠ ∅.
            -- We push @t@ and @t'@ to the stack, and consume the input.
            [((s', t'), (Nothing, Just _))] -> (s', (Just <$> maybeToList t') ++ [t], True)
            -- Case 4: δ(s, t, a) ≠ ∅.
            -- We push @t'@ to the stack (implicitly popping @t@), and consume the input.
            [((s', t'), (Just _, Just _))] -> (s', Just <$> maybeToList t', True)
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
                      ++ " but may only be defined for one of these."
               in error msg
    _transStack (s, Nothing) = s
    _transStack (s, Just t) =
      case SDPDA.stepE pda [] (s, [t]) of
        Left cs ->
          let ss = fst <$> cs
           in fromMaybe (NE.head ss) $ find (`Set.member` _final) ss
        Right ss -> fromMaybe (NE.head ss) $ find (`Set.member` _final) ss

toSipserDPDA :: FPDA s a t -> SipserDPDA s a t
toSipserDPDA pda = error "TODO: implement"

{- Bibliography
 - ~~~~~~~~~~~~
 - [1]: T. D. Randløv, “Toward a Monadic Functional Machine Model for Computability and Complexity Theory: Finite and Pushdown Automata,” Master’s Thesis, University of Copenhagen, 2023.
-}
