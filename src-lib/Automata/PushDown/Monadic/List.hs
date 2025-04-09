{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLists #-}

-- | Monadic pushdown automaton with the `List` monad.
-- Equivalent to a non-deterministic PDA.
module Automata.PushDown.Monadic.List where

import Automata.PushDown.Monadic
import Automata.PushDown.SipserNPDA (SipserNPDA (SipserNPDA))
import qualified Automata.PushDown.SipserNPDA as SNPDA
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

-- | TODO: Fix this
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

-- | TODO: Implement & test that this works.
toSipserNPDA :: ListPDA s a t -> SipserNPDA s a t
toSipserNPDA m = error "TODO: implement"
