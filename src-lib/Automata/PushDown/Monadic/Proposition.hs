{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}

-- | Monadic pushdown automaton with the `Proposition` monad.
-- Equivalent to an alternating pushdown automaton,
-- augmented with negation as well.
module Automata.PushDown.Monadic.Proposition
  ( PropositionPDA,
    accepts,
    invert,
    union,
    intersection,
    concatenate,
    concatenateMarked,
  )
where

import Automata.PushDown.Monadic
import Control.Arrow
import Data.Logic.Proposition
import qualified Data.Set as Set
import Data.These

type PropositionPDA r p a t = MonadicPDA Proposition r p a t

accepts :: (Ord r) => PropositionPDA r p a t -> [a] -> Bool
accepts m w = acceptance $ runMPDA m w
  where
    acceptance = evaluate id

invert :: (Ord r) => PropositionPDA r p a t -> PropositionPDA (Maybe r) p a t
invert m =
  m
    { startState = _startState,
      finalStates = _finalStates,
      transRead = _transRead,
      transPop = _transPop
    }
  where
    _startState = Nothing
    _finalStates =
      let fs = finalStates m
          ss = startState m
       in Set.map Just fs
            `Set.union` Set.fromList [Nothing | ss `Set.notMember` fs]
    _transRead =
      (wrap <$>) . \case
        (Nothing, a) -> Not $ transRead m (startState m, a)
        (Just r, a) -> transRead m (r, a)
    _transPop (p, t) = wrap <$> transPop m (p, t)
    wrap = left (first Just)

-- | For PropositionPDA @m1@ and @m2@, both with alphabet Σ,
-- construct a new PropositionPDA @m@ such that, for string @w@ ∈ Σ*,
-- @m@ accepts @w@ with iff. either @m1@ or @m2@ (or both) accept @w@.
union ::
  (Ord r1, Ord r2) =>
  PropositionPDA r1 p1 a t1 ->
  PropositionPDA r2 p2 a t2 ->
  PropositionPDA (Maybe (Either r1 r2)) (Either p1 p2) a (These t1 t2)
union = combinePDAs (:\/:) (||)

-- | For PropositionPDA @m1@ and @m2@, both with alphabet Σ,
-- construct a new PropositionPDA @m@ such that, for string @w@ ∈ Σ*,
-- @m@ accepts @w@ with iff. both @m1@ and @m2@ accept @w@.
intersection ::
  (Ord r1, Ord r2) =>
  PropositionPDA r1 p1 a t1 ->
  PropositionPDA r2 p2 a t2 ->
  PropositionPDA (Maybe (Either r1 r2)) (Either p1 p2) a (These t1 t2)
intersection = combinePDAs (:/\:) (&&)

-- | Concatenate two Proposition PDAs M1 and M2,
-- such that `concatenate M1 M2`
-- recognizes the language { xy | x ∈ Σ*, y ∈ Σ*, M1 accepts x, M2 accepts y }.
--
-- TODO: Test
concatenate ::
  (Ord r1, Ord r2) =>
  PropositionPDA r1 p1 a t1 ->
  PropositionPDA r2 p2 a t2 ->
  PropositionPDA
    (Either r1 r2)
    (Maybe (Either p1 p2))
    a
    (Either t1 t2)
concatenate m1 m2 =
  MonadicPDA
    { startSymbol = _startSymbol,
      startState = _startState,
      finalStates = _finalStates,
      transRead = _transRead,
      transPop = _transPop
    }
  where
    _startSymbol = Left $ startSymbol m1
    _startState = Left $ startState m1
    _finalStates = Set.map Right (finalStates m2)
    _transRead = \case
      -- In a read state from M1 with input a, simulate δ_read of M1.
      -- Additionally, if we are in a final state, non-deterministically
      -- start simulating M2 on the remaining input as well.
      (Left r, a) ->
        let resL = transRead m1 (r, a)
            resR = pure $ Left (startState m2, [startSymbol m2])
         in if r `Set.member` finalStates m1
              then (wrapL <$> resL) :\/: (wrapR <$> resR)
              else wrapL <$> resL
      -- In a read state from M2 with input a, simulate δ_read of M2.
      (Right r, a) -> wrapR <$> transRead m2 (r, a)
    _transPop = \case
      -- In a pop state from M1 with stack symbol from M1.
      -- We simulate δ_pop of M1.
      (Just (Left p), Left t) -> wrapL <$> transPop m1 (p, t)
      -- In a pop state from M2 with stack symbol from M2.
      -- We simulate δ_pop of M2.
      (Just (Right p), Right t) -> wrapR <$> transPop m2 (p, t)
      -- In a pop state from M1 with stack symbol from M2.
      -- This should be impossible, so we raise an exception
      -- if it somehow does happen.
      (Just (Left _), Right _) ->
        let msg =
              "Automata.PushDown.Monadic.Proposition.concatenate:"
                <> " In pop state of M1 with stack symbol from M2."
                <> " This should not be possible."
         in error msg
      -- In a pop state from M2 with stack symbol from M1.
      -- This means we have effectively reached the bottom
      -- of the stack of M2, and so we reject the input.
      (Just (Right _), Left _) -> pure dead
      -- A dead state.
      (Nothing, _) -> pure dead
    wrapL = (Left *** fmap Left) +++ (Just . Left)
    wrapR = (Right *** fmap Right) +++ (Just . Right)
    dead = Right Nothing

-- | Concatenate two Proposition PDAs M1 and M2 with a dedicated
-- marker symbol `#` between, such that `concatenateMarked M1 M2`
-- recognizes the language { x#y | x ∈ Σ*, y ∈ Σ*, M1 accepts x, M2 accepts y }.
--
-- TODO: Test
concatenateMarked ::
  (Ord r1, Ord r2) =>
  PropositionPDA r1 p1 a t1 ->
  PropositionPDA r2 p2 a t2 ->
  PropositionPDA
    (Either r1 r2)
    (Maybe (Either p1 p2))
    (Maybe a)
    (Either t1 t2)
concatenateMarked m1 m2 =
  MonadicPDA
    { startSymbol = _startSymbol,
      startState = _startState,
      finalStates = _finalStates,
      transRead = _transRead,
      transPop = _transPop
    }
  where
    _startSymbol = Left $ startSymbol m1
    _startState = Left $ startState m1
    _finalStates = Set.map Right (finalStates m2)
    _transRead = \case
      -- In a read state from M1 with input a, simulate δ_read of M1.
      (Left r, Just a) -> wrapL <$> transRead m1 (r, a)
      -- In a read state from M2 with input a, simulate δ_read of M2.
      (Right r, Just a) -> wrapR <$> transRead m2 (r, a)
      -- In a read state from M1 with marker symbol,
      -- go to the start state of M2 with its start symbol
      -- iff. the read state is a final state.
      -- Otherwise, reject the input.
      (Left r, Nothing)
        | r `Set.member` finalStates m1 ->
            let q = startState m2
                ts = [startSymbol m2]
             in pure $ wrapR $ Left (q, ts)
        | otherwise -> pure dead
      -- In a read state from M2 with the marker symbol,
      -- we reject the input.
      (Right _, Nothing) -> pure dead
    _transPop = \case
      -- In a pop state from M1 with stack symbol from M1.
      -- We simulate δ_pop of M1.
      (Just (Left p), Left t) -> wrapL <$> transPop m1 (p, t)
      -- In a pop state from M2 with stack symbol from M2.
      -- We simulate δ_pop of M2.
      (Just (Right p), Right t) -> wrapR <$> transPop m2 (p, t)
      -- In a pop state from M1 with stack symbol from M2.
      -- This should be impossible, so we raise an exception
      -- if it somehow does happen.
      (Just (Left _), Right _) ->
        let msg =
              "Automata.PushDown.Monadic.Proposition.concatenateMarked:"
                <> " In pop state of M1 with stack symbol from M2."
                <> " This should not be possible."
         in error msg
      -- In a pop state from M2 with stack symbol from M1.
      -- This means we have effectively reached the bottom
      -- of the stack of M2, and so we reject the input.
      (Just (Right _), Left _) -> pure dead
      -- A dead state.
      (Nothing, _) -> pure dead
    wrapL = (Left *** fmap Left) +++ (Just . Left)
    wrapR = (Right *** fmap Right) +++ (Just . Right)
    dead = Right Nothing

-- | Generalized function for combining Proposition PDAs,
-- e.g. for implementing union (with '(:\/:)' and '(||)')
-- or intersection (with '(:/\:)' and '(&&)').
combinePDAs ::
  (Ord r1, Ord r2) =>
  (forall x. Proposition x -> Proposition x -> Proposition x) ->
  (Bool -> Bool -> Bool) ->
  PropositionPDA r1 p1 a t1 ->
  PropositionPDA r2 p2 a t2 ->
  PropositionPDA (Maybe (Either r1 r2)) (Either p1 p2) a (These t1 t2)
combinePDAs combP combB m1 m2 =
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
    -- and potentially the designated 'Nothing' start state,
    -- depending on if the start states of M1 or M2 is a final state
    -- (determined with `combB`).
    _finalStates =
      let ssL = startState m1
          ssR = startState m2
          fsL = finalStates m1
          fsR = finalStates m2
       in Set.fromList [_startState | (ssL `Set.member` fsL) `combB` (ssR `Set.member` fsR)]
            `Set.union` Set.map (Just . Left) fsL
            `Set.union` Set.map (Just . Right) fsR
    _transRead = \case
      -- When reading an input symbol in the designated start state,
      -- we non-deterministically choose the read transition from
      -- either M1 or M2 in their respecitve start states,
      -- and combine the result with `combP`.
      (Nothing, a) ->
        let (ql, qr) = (startState m1, startState m2)
         in transReadL (ql, a) `combP` transReadR (qr, a)
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
      (Left _, That _) ->
        let msg =
              "Automata.PushDown.Monadic.Proposition.combinePDAs: "
                <> "In pop state of M1 with only stack symbol from M2."
         in error msg
      -- ... and similarly for M2.
      (Right _, This _) ->
        let msg =
              "Automata.PushDown.Monadic.Proposition.combinePDAs: "
                <> "In pop state of M2 with only stack symbol from M1."
         in error msg
    -- Helper functions
    transReadL = transL (transRead m1)
    transReadR = transR (transRead m2)
    transPopL = transL (transPop m1)
    transPopR = transR (transPop m2)
    transL δ = fmap (((Just . Left) *** fmap (eitherThese . Left)) +++ Left) . δ
    transR δ = fmap (((Just . Right) *** fmap (eitherThese . Right)) +++ Right) . δ
    eitherThese :: Either a b -> These a b
    eitherThese = either This That
