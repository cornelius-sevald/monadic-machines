{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TupleSections #-}

-- | Monadic pushdown automaton with the `Probability` monad.
-- Equivalent to an probabilistic pushdown automaton.
module Automata.PushDown.Monadic.Probability
  ( ProbabilityPDA,
    accepts,
    accepts',
    invert,
    concatenateMarked,
    fromAngelicListPDA,
  )
where

import Automata.PushDown.Monadic
import Automata.PushDown.Monadic.List (ListPDA)
import Automata.PushDown.Util
import Control.Arrow
import qualified Data.Set as Set
import Data.Universe.Class (Finite (universeF))
import Numeric.Probability.Distribution (T)
import qualified Numeric.Probability.Distribution as Dist

type ProbabilityPDA prob r p a t = MonadicPDA (T prob) r p a t

-- | Acceptance function for PPDA, with strict comparison.
--
-- Takes a *cut-point* 0 ≤ η < 1 which is the threshold
-- at which words are accepted, i.e. a larger value
-- of η means that the automaton is less likely to accepts.
--
-- NOTE: If running the PDA results in an "empty" probability,
-- this will throw an error. However, you should not make empty
-- transitions in the probability monad anyway, as it results
-- in invalid probabilities when running the PDA, i.e. you may
-- get results where P(accept) + P(reject) < 1.
accepts ::
  (Ord r, Ord prob, Num prob) =>
  ProbabilityPDA prob r p a t ->
  prob ->
  [a] ->
  Bool
accepts m η w = acceptance $ runMPDA m w
  where
    acceptance t = Dist.truth t > η

-- | Alternative acceptance function for PPDA, using non-strict comparison.
--
-- Takes a *cut-point* 0 ≤ η ≤ 1 which is the threshold
-- at which words are accepted, i.e. a larger value
-- of η means that the automaton is less likely to accepts.
accepts' ::
  (Ord r, Ord prob, Num prob) =>
  ProbabilityPDA prob r p a t ->
  prob ->
  [a] ->
  Bool
accepts' m η w = acceptance $ runMPDA m w
  where
    acceptance t = Dist.truth t >= η

-- | Invert PPDA M, such that `invert M` accepts a word `w`
-- with non-strict cut-point η iff. `M` rejects `w` with
-- strict cut-point (1-η).
--
-- NOTE: NO NO NO NO NO
invert ::
  (Finite r, Ord r, Fractional prob) =>
  ProbabilityPDA prob r p a t ->
  ProbabilityPDA prob (Maybe (Maybe r)) (Either p p) a (Bottomed t)
invert m =
  onPopEmptyStack (const $ pure (accept, [])) $
    MonadicPDA
      { startSymbol = _startSymbol,
        startState = _startState,
        finalStates = _finalStates,
        transRead = _transRead,
        transPop = _transPop
      }
  where
    _startSymbol = startSymbol m
    _startState = Just $ startState m
    _finalStates = Set.insert accept $ Set.map Just $ complement (finalStates m)
    _transRead = \case
      (Nothing, _) -> pure $ Left (accept, [])
      (Just r, a) -> wrap $ transRead m (r, a)
    _transPop (p, t) = wrap $ transPop m (p, t)
    wrap = fmap $ left (first Just)
    accept = Nothing
    complement = (Set.fromList universeF `Set.difference`)

-- | Concatenate two PPDAs M1 and M2 with a dedicated
-- marker symbol `#` between, such that `concatenateMarked M1 M2`
-- recognizes, with cut-point η the language
-- { x#y | x ∈ Σ*
--       , y ∈ Σ*
--       , M1 accepts x w. cut-point η₁
--       , M2 accepts y w. cut point η₂
--       , η ≤ η₁ * η₂
-- }
--
-- While the CFL is closed under concatenation,
-- I don't believe that this is the case for the stochastic CFL,
-- and so we would need a dedicated marker symbol.
--
-- TODO: Test
concatenateMarked ::
  (Num prob, Ord r1, Ord r2) =>
  ProbabilityPDA prob r1 p1 a t1 ->
  ProbabilityPDA prob r2 p2 a t2 ->
  ProbabilityPDA
    prob
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
              "Automata.PushDown.Monadic.Probability.concatenateMarked:"
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

-- | Given a ListPDA `m` and cut-point `η`,
-- construct a ProbabilityPDA `m'` such that
-- `m` accepts a word `w` with angelic non-determinism iff.
-- `m'` accepts `w#` with cut-point `η`, where `#` is an end-of-input marker.
fromAngelicListPDA ::
  (Ord r, Fractional prob) =>
  prob ->
  ListPDA r p a t ->
  ProbabilityPDA
    prob
    (Maybe (Either r (Bool, Bool)))
    (Either p p)
    (Ended a)
    (Bottomed t)
fromAngelicListPDA η m =
  onPopEmptyStack (const $ rejectP False) $
    MonadicPDA
      { startSymbol = _startSymbol,
        startState = _startState,
        finalStates = _finalStates,
        transRead = _transRead,
        transPop = _transPop
      }
  where
    _startSymbol = startSymbol m
    _startState = Left $ startState m
    _finalStates = [accept True, accept False]
    _transRead = \case
      -- If we are in a terminal state (either the `accept` or `reject` states)
      -- and we have not read the end-of-input symbol,
      -- then the PDA failed for another reason
      -- (popping empty stack or empty result from transition).
      -- In this case, we ignore any further input and stay in
      -- the current terminal state.
      (Right (b, False), _) -> pure $ Left (Right (b, False), [])
      -- If we are in a terminal state
      -- and we *have* read the end-of-input symbol,
      -- then any further input should lead to rejection.
      (Right (_, True), _) -> pure $ Left (Right (False, True), [])
      (Left r, ISymbol a) -> case uniform $ transRead m (r, a) of
        Left x -> Left <$> x
        Right cs -> do
          c <- cs
          case c of
            Left (r', ts) -> pure $ Left (Left r', ts)
            Right p' -> pure $ Right p'
      (Left r, End) ->
        if r `Set.member` finalStates m
          then pure $ Left (accept True, [])
          else Left <$> rejectP True
    _transPop (p, t) =
      case uniform $ transPop m (p, t) of
        Left x -> Left <$> x
        Right cs -> do
          c <- cs
          pure $ case c of
            Left (r', ts) -> Left (Left r', ts)
            Right p' -> Right p'
    -- Dedicated accept state.
    -- The boolean signifies if we have read the end-of-input symbol.
    accept b = Right (True, b)
    -- Dedicated reject state.
    -- The boolean signifies if we have read the end-of-input symbol.
    reject b = Right (False, b)
    withEta p q = Dist.fromFreqs [(p, η), (q, 1 - η)]
    -- Reject with probability (1-η), and otherwise accept.
    -- The boolean signifies if we have read the end-of-input symbol.
    rejectP b = (,[]) <$> withEta (accept b) (reject b)
    uniform xs =
      if null xs
        then Left $ rejectP False
        else Right $ Dist.uniform xs
