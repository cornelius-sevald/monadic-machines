{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

-- | Monadic pushdown automaton with the `List` monad.
-- Equivalent to a non-deterministic PDA.
module Automata.PushDown.Monadic.List where

import Automata.PushDown.FPDA (PopState (..), ReadState (..))
import Automata.PushDown.Monadic
import Automata.PushDown.SipserNPDA (SipserNPDA (SipserNPDA))
import qualified Automata.PushDown.SipserNPDA as SNPDA
import Automata.PushDown.Util (Bottomed (..))
import Control.Applicative (Alternative (empty))
import Control.Arrow
import Control.Monad (foldM)
import qualified Data.Set as Set
import Data.These
import Data.Universe.Class (Finite (..))

type ListPDA r p a t = MonadicPDA [] r p a t

acceptsAngelig :: (Ord r) => ListPDA r p a t -> [a] -> Bool
acceptsAngelig m w = acceptance $ runMPDA m w
  where
    acceptance = or

acceptsDemonic :: (Ord r) => ListPDA r p a t -> [a] -> Bool
acceptsDemonic m w = acceptance $ runMPDA m w
  where
    acceptance = and

-- Invert a List PDA, such that List PDA @m@ accepts string @w@
-- with angelic non-determinism iff. @invert m@ rejects @w@
-- with *demonic* non-determinism, or the other way around.
invert :: (Ord r, Finite r) => ListPDA r p a t -> ListPDA r p a t
invert m = m {finalStates = complement $ finalStates m}
  where
    complement xs = Set.fromList universeF `Set.difference` xs

-- | For a ListPDA @m1@ and @m2@, both with alphabet Σ,
-- construct a new ListPDA @m@ such that, for string @w@ ∈ Σ*,
-- @m@ accepts @w@ with angelic non-determinism iff.
-- either @m@ or @m@ (or both) accept @m@ with angelic non-determinism.
--
-- TODO: Test, and see if I can make a similar 'intersect' function for
-- demonic non-determinism.
union ::
  (Ord r1, Ord r2) =>
  ListPDA r1 p1 a t1 ->
  ListPDA r2 p2 a t2 ->
  ListPDA (Maybe (Either r1 r2)) (Either p1 p2) a (These t1 t2)
union m1 m2 =
  MonadicPDA
    { startSymbol = _startSymbol,
      startState = _startState,
      finalStates = _finalStates,
      transRead = _transRead,
      transPop = _transPop
    }
  where
    -- We use a designated start state 'Nothing'.
    _startState = Nothing
    -- As we may need the start symbol of either M1 or M2,
    -- we put both as the designated start symbol.
    _startSymbol = These (startSymbol m1) (startSymbol m2)
    -- The final states is the union of the final states of M1 and M2,
    -- as well as the designated 'Nothing' start state if either of
    -- the start states of M1 or M2 is a final state.
    _finalStates =
      let ssL = startState m1
          ssR = startState m2
          fsL = finalStates m1
          fsR = finalStates m2
       in Set.map (Just . Left) fsL
            `Set.union` Set.map (Just . Right) fsR
            `Set.union` Set.fromList [_startState | ssL `Set.member` fsL || ssR `Set.member` fsR]
    _transRead = \case
      -- When reading an input symbol in the designated start state,
      -- we non-deterministically choose the read transition from
      -- either M1 or M2 in their respecitve start states.
      (Nothing, a) ->
        let (ql, qr) = (startState m1, startState m2)
         in transReadL (ql, a) <> transReadR (qr, a)
      -- When in a read state in M1, we use the read transition function of M1...
      (Just (Left q), a) -> transReadL (q, a)
      -- ... and conversely for M2.
      (Just (Right q), a) -> transReadR (q, a)
    _transPop = \case
      -- When in a pop state in M1, we use the pop transition function of M1,
      -- using a stack symbol from M1.
      (Left q, This t) -> transPopL (q, t)
      -- If there is both a stack symbol from M1 and M2,
      -- we discard the stack symbol from M2...
      (Left q, These t _) -> transPopL (q, t)
      -- ... and conversely for M2.
      (Right q, That t) -> transPopR (q, t)
      (Right q, These _ t) -> transPopR (q, t)
      -- It should not be possible to be in a situation where
      -- we are in a pop state of M1 with only a stack symbol from M2.
      (Left _, That _) -> error "In pop state of M1 with only stack symbol from M2."
      -- ... and similarly for M2.
      (Right _, This _) -> error "In pop state of M2 with only stack symbol from M1."
    -- Helper functions
    transReadL = transL (transRead m1)
    transReadR = transR (transRead m2)
    transPopL = transL (transPop m1)
    transPopR = transR (transPop m2)
    transL δ = map (((Just . Left) *** map (eitherThese . Left)) +++ Left) . δ
    transR δ = map (((Just . Right) *** map (eitherThese . Right)) +++ Right) . δ
    eitherThese :: Either a b -> These a b
    eitherThese = either This That

-- | Convert a List PDA to a Sipser NPDA.
--
-- See 'Automata.PushDown.FPDA.toSipserDPDA'
-- for a description on how it works.
toSipserNPDA :: (Ord r, Ord p, Ord t) => ListPDA r p a t -> SipserNPDA (Maybe (Either r p)) a t
toSipserNPDA m =
  SipserNPDA
    { SNPDA.startState = _startState,
      SNPDA.finalStates = _finalStates,
      SNPDA.trans = _trans
    }
  where
    -- Mark a state as a read state
    readState = Just . Left
    _startState = Nothing
    _finalStates = Set.map readState (finalStates m)
    _trans =
      Set.fromList . \case
        (Nothing, Nothing, Nothing) ->
          let q = readState $ startState m
              t = startSymbol m
           in pure (q, [t])
        (Just (Left r), Nothing, Just a) -> do
          (q, ts) <- collect <$> transRead m (r, a)
          pure (Just q, ts)
        (Just (Right p), Just t, Nothing) -> do
          (q, ts) <- collect <$> transPop m (p, t)
          pure (Just q, ts)
        (_, _, _) -> empty
    collect :: Either (r, [t]) p -> (Either r p, [t])
    collect (Left (r, ts)) = (Left r, ts)
    collect (Right p) = (Right p, [])

-- | Convert a Sipser NPDA to a List PDA.
--
-- See 'Automata.PushDown.FPDA.fromSipserDPDA'
-- for a description on how it works.
fromSipserNPDA ::
  forall s a t.
  (Finite s, Finite a, Finite t, Ord s, Ord t) =>
  SipserNPDA s a t ->
  ListPDA (ReadState s, Bool) (PopState s a, Bool) a (Bottomed t)
fromSipserNPDA pda = listPDA
  where
    listPDA =
      MonadicPDA
        { startSymbol = _startSymbol,
          startState = _startState,
          finalStates = _finalStates,
          transRead = _transRead,
          transPop = _transPop
        }
    _startSymbol = Bottom
    _startState =
      -- If a final state is reachable from the start state of the Sipser NPDA
      -- without consuming any input, we want to mark the start state as final.
      let b = finalReachable (SNPDA.startState pda, [])
       in (Start, b)
    -- The final states of the FPDA is the union of the final states
    -- of the Sipser NPDA, and *all* of the states of the NPDA
    -- marked with the "final" boolean flag.
    _finalStates =
      Set.map ((,False) . ReadState) (SNPDA.finalStates pda)
        `Set.union` Set.map (,True) (Set.fromList universeF)
    -- Read transition function.
    _transRead :: ((ReadState s, b), a) -> [Either ((ReadState s, Bool), [Bottomed t]) (PopState s a, Bool)]
    _transRead =
      \case
        ((Start, _), a) -> do
          let q = SNPDA.startState pda
          -- From the start state, we only want to progress if a read state is
          -- reachable from the start state of the Sipser NPDA via ε-transitions.
          Left ((ReadState q', _), ts') <- nextStates (q, [])
          -- From here, we'd like to go directly to a pop state `(q', Just a, False)`,
          -- but this means we can't push `ts'` to the stack!
          -- Instead, we fold `stepPop` over the stack, starting from the read state
          -- (much like in `stepRead`).
          -- This ensures that either the entire stack is consumed by pop-steps,
          -- or we hit a new read state at some point,
          -- and so we can push the remaining stack.
          let f = stepPop listPDA
              p = Right (PopState q' (Just a), False)
          foldM f p ts'
        -- If we are in a stuck state and read any input symbol,
        -- we want to reject the word.
        ((Stuck, _), _) -> []
        -- Otherwise we move to a pop state, which also handles
        -- reading the input `a`.
        ((ReadState q, _), a) -> pure $ Right (PopState q (Just a), False)
    -- Pop transition function.
    _transPop :: ((PopState s a, Bool), Bottomed t) -> [Either ((ReadState s, Bool), [Bottomed t]) (PopState s a, Bool)]
    _transPop =
      \case
        -- Case 1: in state `q` with input symbol `a` and stack symbol `t`.
        ((PopState q (Just a), _), SSymbol t) ->
          stepNPDA [] (q, Just t, Just a) <> stepNPDA [t] (q, Nothing, Just a)
        -- Case 2: in state `q` with input symbol `a` and empty stack.
        ((PopState q (Just a), _), Bottom) -> do
          q' <- stepNPDA [] (q, Nothing, Just a)
          let appendBottom = second (++ [Bottom])
          pure $ left appendBottom q'
        -- Case 3: in state `q` with no input symbol and stack symbol `t`.
        ((PopState q Nothing, b), SSymbol t) -> do
          q' <- stepNPDA [] (q, Just t, Nothing)
          let propBoolL = first $ second (b ||)
          let propBoolR = second (b ||)
          pure $ (propBoolL +++ propBoolR) q'
        -- Case 4: in state `q` with no input symbol and empty stack.
        ((PopState _ Nothing, b), Bottom) ->
          if b then pure $ stuck True else []
    -- Step the Sipser NPDA with configuration `(q, t, a)` and remaining stack `ts`,
    -- and then advance to the next reachable read- or pop states.
    stepNPDA :: [t] -> (s, Maybe t, Maybe a) -> [Either ((ReadState s, Bool), [Bottomed t]) (PopState s a, Bool)]
    stepNPDA ts (q, t, a) = do
      (q', ts') <- Set.toList $ SNPDA.trans pda (q, t, a)
      nextStates (q', ts' ++ ts)
    -- Transitively advance to a state which can't step any further.
    nextStates :: (s, [t]) -> [Either ((ReadState s, Bool), [Bottomed t]) (PopState s a, Bool)]
    nextStates (q, ts) = do
      -- We get either a configuration that can't step any further
      -- without input, or a state in an ε-loop.
      -- In either case, `b` is a boolean indicating that a
      -- final state was encountered.
      ((q', ts'), b) <- Set.toList $ SNPDA.stepE' pda [] ((q, ts), False)
      let p = Right (PopState q' Nothing, b)
          r = Left ((ReadState q', b), SSymbol <$> ts')
       in if null ts' then [p, r] else [r]
    -- The stuck state is a read state which indicates that the Sipser PDA can't
    -- make any more meaningful transitions, which happens either if popping from an empty stack,
    -- or reaching an infinite cycle where we read no input and don't shrink the stack.
    -- The boolean flag indicates if this stuck state should be a final state,
    -- which can e.g. happen if we enter an infinite loop containing a final state.
    stuck :: Bool -> Either ((ReadState s, Bool), [Bottomed t]) x
    stuck b = Left ((Stuck, b), [])
    -- Check if a final state is reachable from state `q` with stack symbols `ts`,
    -- but consuming no input.
    finalReachable (q, ts) =
      let reachable = Set.map fst $ SNPDA.stepE pda [] (q, ts)
       in reachable `intersects` SNPDA.finalStates pda
    -- Is the intersections of `xs` and `ys` non-empty?
    intersects xs ys = not $ xs `Set.disjoint` ys
