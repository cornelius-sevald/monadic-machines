{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLists #-}

-- | Functional Deterministic Pushdown Automata
module Automata.PushDown.FPDA where

import Automata.PushDown.SipserDPDA (EOISipserDPDA, SipserDPDA (SipserDPDA))
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
-- No more input, return current state and stack.
stepsInput _ _ (s, ts, []) = Just (s, ts)
-- Popping from empty stack with remaining input, return error.
stepsInput _ _ (_, [], _) = Nothing
-- Reading input symbol 'a' and popping symbol 't' from stack.
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

-- | Convert a Sipser DPDA to a Functional PDA.
--
-- BUG:
--   1) Automata, Pushdown Automata, Functional Deterministic PDAs, fromSipserDPDA, For a random Sipser DPDA,
--  recognizes the same language
--        Falsifiable (after 76 tests and 45 shrinks):
--          ( Ith# 1
--          , fromList [Ith# 1]
--          , { Ith# 2->Ith# 2
--            , Ith# 4->Ith# 3
--            , Ith# 8->Ith# 2
--            , _->Ith# 1
--            }
--          , { (Ith# 2,Nothing,Just A)->(Ith# 8,[])
--            , (Ith# 4,Just B,Nothing)->(Ith# 4,[B])
--            , (Ith# 4,Just C,Nothing)->(Ith# 1,[])
--            , (Ith# 8,Nothing,Just A)->(Ith# 4,[])
--            , _->(Ith# 2,[B,C])
--            }
--          )
--          [A,A]
--        expected: False
--         but got: True
--  To rerun use: --match "/Automata/Pushdown Automata/Functional Deterministic PDAs/fromSipserDPDA/For a random Sipser DPDA/recognizes the same language/" --seed 1228129009
--
-- TODO: Investigate & fix.
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
    δ = SDPDA.trans pda
    _final = SDPDA.final pda
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
            [((s', t'), (Nothing, Nothing))] -> (s', (Just <$> t') ++ [t], False)
            -- Case 2: δ(s, t, ε) ≠ ∅.
            -- We push @t'@ to the stack (implicitly popping @t@), and consume no input.
            [((s', t'), (Just _, Nothing))] -> (s', Just <$> t', False)
            -- Case 3: δ(s, ε, a) ≠ ∅.
            -- We push @t@ and @t'@ to the stack, and consume the input.
            [((s', t'), (Nothing, Just _))] -> (s', (Just <$> t') ++ [t], True)
            -- Case 4: δ(s, t, a) ≠ ∅.
            -- We push @t'@ to the stack (implicitly popping @t@), and consume the input.
            [((s', t'), (Just _, Just _))] -> (s', Just <$> t', True)
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
    _transStack (s, t) =
      case SDPDA.stepE pda [] (s, maybeToList t) of
        Left cs ->
          let ss = fst <$> cs
           in fromMaybe (NE.head ss) $ find (`Set.member` _final) ss
        Right ss -> fromMaybe (NE.head ss) $ find (`Set.member` _final) ss

-- | Convert a Functional PDA to a Sipser Deterministic PDA.
--
-- This function is a total mess, but the most important thing as that
-- the input alphabet is extended with an end-of-input symbol ('End'),
-- meaning that, if a FPDA accepts a language @lang@,
-- then the corresponding Sipser DPDA will accept the language @['ISymbol' w <> 'End' | w <- lang]@.
--
-- The trouble with converting FPDAs to Sipser DPDAs is that the Sipser DPDA wants to know
-- whether a transition reads the input or is an ε-transition "up front", i.e. before even
-- looking at the input symbol.
-- On the other hand, FPDAs will always read an input symbol, but then after the fact
-- decide if you actually consume the input symbol or simply peek at it.
--
-- The way this conversion roughly works is by creating a state for each state / input symbol pair
-- of the FPDA. This lets us save the previous symbol read, or from a different perspective,
-- it essentially lets us "peek" at the next input symbol, and so we can decide if we
-- read the input or do an ε-transition "up front".
-- Additionally, we also create a new state for each state of the FPDA which is
-- used when we are out of input (the @(s, 'End')@ states).
-- From these we use the 'transStack' function.
-- Finally, we also have a dedicated start state 'Start' and final state 'Final'.
--
-- The states have type @'State' (s, 'Ended' a)@ and should be interpreted as follows:
--   - @'Start'@:                   The start state.
--   - @'Final'@:                   The dedicated final state.
--   - @'Middle' (s, 'ISymbol' a)@: In state @s@ with @a@ as the current input symbol.
--   - @'Right' (s, 'End')@:        In state @s@ with no input left.
--
-- The input alphabet is extended with a dedicated end-of-input symbol ('End'),
-- which should be placed, and only placed, at the end of any input string.
-- The 'Automata.PushDown.SipserDPDA.acceptsEOI' function does this automatically.
--
-- The stack alphabet is also extended with a dedicated bottom-of-stack symbol ('Bottom').
toSipserDPDA :: (Ord s, Ord a) => FPDA s a t -> EOISipserDPDA (State (s, Ended a)) a (Bottomed t)
toSipserDPDA fpda =
  SipserDPDA
    { SDPDA.start = _start,
      SDPDA.final = _final,
      SDPDA.trans = _trans
    }
  where
    δ = transInput fpda
    γ = transStack fpda
    _start = Start
    _final = [Final]
    _trans = \case
      -- From the starting state, we put the start symbol on the stack,
      -- and move to the "real" start state, peeking at input symbol 'p'.
      (Start, Nothing, Just p) ->
        let s = Middle (start fpda, p)
            t = SSymbol $ startSymbol fpda
         in Just (s, [t, Bottom])
      (Start, _, _) -> Nothing
      -- In the final state, there is nothing to do so we stay.
      (Final, Nothing, Nothing) ->
        Just (Final, [])
      (Final, _, _) -> Nothing
      -- This should be treated as being in state 's' and reading 'a'.
      -- If we consume 'a', then this is undefined,
      -- as we have nothing further to read.
      (Middle (s, ISymbol a), Just (SSymbol t), Nothing) ->
        let (s', t', consume) = δ (s, t, a)
         in if consume
              then Nothing
              else Just (Middle (s', ISymbol a), SSymbol <$> t')
      -- This should be treated as being in state 's' and reading 'a',
      -- while peeking at the next input 'p' (which might be the end-of-input marker).
      -- If we don't consume 'a', then this is undefined
      -- as we then don't want to immediately read 'p'.
      (Middle (s, ISymbol a), Just (SSymbol t), Just p) ->
        let (s', t', consume) = δ (s, t, a)
         in if not consume
              then Nothing
              else Just (Middle (s', p), SSymbol <$> t')
      -- Here we are trying to pop from a (morally) empty stack,
      -- and so we are stuck.
      (Middle (s, ISymbol a), Just Bottom, _) ->
        Just (Middle (s, ISymbol a), [])
      (Middle (_, ISymbol _), _, _) -> Nothing
      -- The '(s, End)' indicates that we have read all input,
      -- and so we only move on the stack symbols.
      (Middle (s, End), Just (SSymbol t), Nothing) ->
        let s' = γ (s, t)
         in Just (Middle (s', End), [])
      -- If we have read all input and the stack is empty,
      -- we check if we are in a final state (of the FPDA).
      -- If so, we move to the dedicated final state 'Final' (of the SDPDA).
      -- Otherwise, we want to reject the input, so we stay in this state.
      (Middle (s, End), Just Bottom, Nothing)
        | s `Set.member` final fpda -> Just (Final, [])
        | otherwise -> Just (Middle (s, End), [])
      (Middle (_, End), _, _) -> Nothing

{- Bibliography
 - ~~~~~~~~~~~~
 - [1]: T. D. Randløv, “Toward a Monadic Functional Machine Model for Computability and Complexity Theory: Finite and Pushdown Automata,” Master’s Thesis, University of Copenhagen, 2023.
-}
