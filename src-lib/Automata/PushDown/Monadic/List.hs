{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLists #-}

-- | Monadic pushdown automaton with the `List` monad.
-- Equivalent to a non-deterministic PDA.
module Automata.PushDown.Monadic.List where

import Automata.PushDown.Monadic
import Automata.PushDown.SipserNPDA (SipserNPDA (SipserNPDA))
import qualified Automata.PushDown.SipserNPDA as SNPDA
import Automata.PushDown.Util
import Data.Foldable (find)
import Data.Maybe (maybeToList)
import qualified Data.Set as Set

type ListPDA s a t = MonadicPDA [] s a t

acceptsAngelig :: (Ord s, Ord a, Ord t) => ListPDA s a t -> [a] -> Bool
acceptsAngelig m w = acceptance $ runMPDA m w
  where
    acceptance = or

acceptsDemonic :: (Ord s, Ord a, Ord t) => ListPDA s a t -> [a] -> Bool
acceptsDemonic m w = acceptance $ runMPDA m w
  where
    acceptance = and

fromSipserNPDA :: (Ord s, Ord t) => SipserNPDA s a t -> ListPDA s a (Maybe t)
fromSipserNPDA pda =
  MonadicPDA
    { start = SNPDA.start pda,
      startSymbol = Nothing,
      final = _final,
      transInput = _transInput,
      transStack = _transStack
    }
  where
    splitMaybe x = case x of Nothing -> [Nothing]; Just y -> [Nothing, Just y]
    δ = SNPDA.trans pda
    _final = SNPDA.final pda
    _transInput (s, t, a) =
      let input = (,) <$> splitMaybe t <*> splitMaybe (Just a)
          cs = (\(t', a') -> (s, t', a')) <$> input
          cs' = do
            (c, i) <- zip cs input
            c' <- Set.toList $ δ c
            pure (c', i)
          go c = case c of
            -- Case 1: δ(s, ε, ε) ≠ ∅.
            -- We push @t@ and @t'@ to the stack, and consume no input.
            ((s', t'), (Nothing, Nothing)) -> (s', (Just <$> t') ++ [t], False)
            -- Case 2: δ(s, t, ε) ≠ ∅.
            -- We push @t'@ to the stack (implicitly popping @t@), and consume no input.
            ((s', t'), (Just _, Nothing)) -> (s', Just <$> t', False)
            -- Case 3: δ(s, ε, a) ≠ ∅.
            -- We push @t@ and @t'@ to the stack, and consume the input.
            ((s', t'), (Nothing, Just _)) -> (s', (Just <$> t') ++ [t], True)
            -- Case 4: δ(s, t, a) ≠ ∅.
            -- We push @t'@ to the stack (implicitly popping @t@), and consume the input.
            ((s', t'), (Just _, Just _)) -> (s', Just <$> t', True)
       in go <$> cs'
    _transStack (s, t) =
      let ts = maybeToList t
          -- We get a list of all configurations reachable
          -- with no input and only `t` (if present) on the stack.
          cs = Set.toList (SNPDA.stepE pda [] (s, ts))
       in case find (`Set.member` _final) (fst <$> cs) of
            -- If any final state is reachable, we stay in that state,
            -- which will eventually accept the string once the entire
            -- remaining stack is read.
            Just s' -> pure s'
            -- If no final state is reachable, we filter out
            -- any configuration with remaining stack symbols, as they are stuck
            -- (as they have not read more from the stack than they have written).
            Nothing -> fst <$> filter (null . snd) cs

-- | TODO: Test that this works.
toSipserNPDA ::
  (Ord s, Ord a, Ord t) =>
  ListPDA s a t ->
  SipserNPDA (State (s, Ended a)) a (Bottomed t)
toSipserNPDA m =
  SipserNPDA
    { SNPDA.start = _start,
      SNPDA.final = _final,
      SNPDA.trans = _trans
    }
  where
    δ = transInput m
    γ = transStack m
    _start = Start
    _final = [Final]
    _trans =
      Set.fromList . \case
        -- From the starting state, we put the start symbol on the stack,
        -- and move to the "real" start state,
        -- peeking at input symbol 'a' (which might be missing).
        (Start, Nothing, a) ->
          let a' = maybe End ISymbol a
              s = Middle (start m, a')
              t = SSymbol $ startSymbol m
           in pure (s, [t, Bottom])
        (Start, _, _) -> []
        -- In the final state, there is nothing to do so we stay.
        (Final, Nothing, Nothing) -> pure (Final, [])
        (Final, _, _) -> []
        -- This should be treated as being in state 's' and reading 'a'.
        -- If we consume 'a', then this is undefined,
        -- as we have nothing further to read.
        (Middle (s, ISymbol a), Just (SSymbol t), Nothing) -> do
          (s', t', consume) <- δ (s, t, a)
          if consume
            then pure (Middle (s', End), SSymbol <$> t')
            else pure (Middle (s', ISymbol a), SSymbol <$> t')
        -- This should be treated as being in state 's' and reading 'a',
        -- while peeking at the next input 'p' (which might be the end-of-input marker).
        -- If we don't consume 'a', then this is undefined
        -- as we then don't want to immediately read 'p'.
        (Middle (s, ISymbol a), Just (SSymbol t), Just p) -> do
          (s', t', consume) <- δ (s, t, a)
          if not consume
            then []
            else pure (Middle (s', ISymbol p), SSymbol <$> t')
        -- Here we are trying to pop from a (morally) empty stack,
        -- and so we are stuck.
        (Middle (s, ISymbol a), Just Bottom, _) -> pure (Middle (s, ISymbol a), [])
        (Middle (_, ISymbol _), _, _) -> []
        -- The '(s, End)' indicates that we have read all input,
        -- and so we only move on the stack symbols.
        (Middle (s, End), Just (SSymbol t), Nothing) -> do
          s' <- γ (s, t)
          pure (Middle (s', End), [])
        -- If we have read all input and the stack is empty,
        -- we check if we are in a final state (of the List PDA).
        -- If so, we move to the dedicated final state 'Final' (of the SNPDA).
        -- Otherwise, we want to reject the input, so we stay in this state.
        (Middle (s, End), Just Bottom, Nothing)
          | s `Set.member` final m -> pure (Final, [])
          | otherwise -> pure (Middle (s, End), [])
        (Middle (_, End), _, _) -> []
