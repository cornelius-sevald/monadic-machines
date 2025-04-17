{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

-- | Functional Deterministic Pushdown Automata
module Automata.PushDown.FPDA where

import Automata.PushDown.SipserDPDA (SipserDPDA (SipserDPDA))
import qualified Automata.PushDown.SipserDPDA as SDPDA
import Automata.PushDown.Util (Bottomed (..))
import Control.Monad (foldM)
import Data.Foldable (toList)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe (catMaybes, isJust)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Universe.Class (Finite (..), Universe (..))

-- | A Functional Pushdown Automaton (FPDA),
-- is a 9-tuple (P, R, Σ, Γ, δ_read, δ_pop, Z₀, q₀, F) where,
--
--   - P, R, Σ, Γ are finite sets, called the *pop states*,
--     *read states*, *input symbols* and *stack symbols*, resp.
--   - δ_read : R × Σ → P is the *read transition function*,
--   - δ_pop  : P × Γ → P + (R × Γ^*) is the *pop transition function*,
--   - Z₀ ∈ Γ is the *start symbol*, and
--   - r_0 ∈ R is the *start state*, and
--   - F ⊆ R is the set of *accepting states*.
--
-- The notation Γ^* denotes the set {ε} ∪ Γ ∪ (Γ × Γ) ∪ ....
--
-- The read- and pop states, and, input- and stack alphabet
-- is implicitly given by the types `r`, `p`, `a`, `t` respectively.
data FPDA r p a t = FPDA
  { startSymbol :: t,
    startState :: r,
    finalStates :: Set r,
    transRead :: (r, a) -> Either (r, [t]) p,
    transPop :: (p, t) -> Either (r, [t]) p
  }

catch :: Either x p -> Maybe x
catch (Left x) = Just x
catch (Right _) = Nothing

stepPop :: FPDA r p a t -> Either (r, [t]) p -> t -> Either (r, [t]) p
stepPop _ (Left (r, ts)) t = Left (r, ts <> [t])
stepPop fpda (Right p) t = transPop fpda (p, t)

stepRead :: FPDA r p a t -> (r, [t]) -> a -> Maybe (r, [t])
stepRead fpda (r, ts) a =
  let s = transRead fpda (r, a)
   in catch $ foldl (stepPop fpda) s ts

steps :: FPDA r p a t -> [a] -> (r, [t]) -> Maybe (r, [t])
steps fpda as c = foldM (stepRead fpda) c as

accepts :: (Ord r) => FPDA r p a t -> [a] -> Bool
accepts fpda as =
  let c_0 = (startState fpda, [startSymbol fpda])
   in case steps fpda as c_0 of
        Nothing -> False
        Just (q, _) -> q `Set.member` finalStates fpda

-- | Convert a Functional PDA to a Sipser Deterministic PDA.
--
-- We let the states be Q = (R + P) ∪ {q₀}, where q₀ is a designated start state.
-- From q₀ we put the start symbol Z₀ on the stack, and then move to the
-- start state of the FPDA, i.e. we define δ(q₀, ε, ε) = (r₀, [Z₀]).
-- For a read state r, we define
--   δ(r, ε, a) = (δ_read(r, a), ε)   if δ_read(r, a) is a pop state,
--              =  δ_read(r, a)       otherwise.
-- For a pop state p, we define
--   δ(p, t, ε) = (δ_pop(p, t), ε)    if δ_pop(p, t) is a pop state,
--              =  δ_pop(p, t)        otherwise.
-- We leave δ undefined for all other inputs.
toSipserDPDA :: (Ord r, Ord p) => FPDA r p a t -> SipserDPDA (Maybe (Either r p)) a t
toSipserDPDA fpda =
  SipserDPDA
    { SDPDA.startState = _startState,
      SDPDA.finalStates = _finalStates,
      SDPDA.trans = _trans
    }
  where
    -- Mark a state as a read state
    readState = Just . Left
    _startState = Nothing
    _finalStates = Set.map readState (finalStates fpda)
    _trans = \case
      (Nothing, Nothing, Nothing) ->
        let q = readState $ startState fpda
            t = startSymbol fpda
         in Just (q, [t])
      (Just (Left r), Nothing, Just a) ->
        let (q, ts) = collect $ transRead fpda (r, a)
         in Just (Just q, ts)
      (Just (Right p), Just t, Nothing) ->
        let (q, ts) = collect $ transPop fpda (p, t)
         in Just (Just q, ts)
      (_, _, _) -> Nothing
    collect :: Either (r, [t]) p -> (Either r p, [t])
    collect (Left (r, ts)) = (Left r, ts)
    collect (Right p) = (Right p, [])

-- | Type of read state used in 'fromSipserDPDA'.
-- Has a dedicated start state and "stuck" state,
-- meaining that the DPDA has no more valid moves,
-- e.g. if it pops an empty stack.
data ReadState s
  = Start
  | ReadState s
  | Stuck
  deriving (Show, Eq, Ord)

-- | Type of pop state used in 'fromSipserDPDA'.
-- It is the cartesian product of a state 's',
-- and an optional input symbol 'a'.
-- This is so that the FPDA can simulate a
-- transition δ(s, t, a) of the DPDA,
-- that needs to read both the next input symbol
-- and top stack symbol at the same time.
data PopState s a = PopState s (Maybe a)
  deriving (Show, Eq, Ord)

instance (Universe s) => Universe (ReadState s) where
  universe = Start : Stuck : map ReadState universe

instance (Finite s) => Finite (ReadState s)

-- | Convert a Sipser DPDA to a Functional PDA.
--
-- TODO: Explain the inner machinations of this beast.
fromSipserDPDA ::
  forall s a t.
  (Finite s, Finite a, Finite t, Ord s, Eq t) =>
  SipserDPDA s a t ->
  FPDA (ReadState s, Bool) (PopState s a, Bool) a (Bottomed t)
fromSipserDPDA pda = fpda
  where
    fpda =
      FPDA
        { startSymbol = _startSymbol,
          startState = _startState,
          finalStates = _finalStates,
          transRead = _transRead,
          transPop = _transPop
        }
    _startSymbol = Bottom
    _startState =
      -- If a final state is reachable from the start state of the Sipser DPDA
      -- without consuming any input, we want to mark the start state as final.
      let b = finalReachable (SDPDA.startState pda, [])
       in (Start, b)
    -- The final states of the FPDA is the union of the final states
    -- of the Sipser DPDA, and *all* of the states of the FPDA
    -- marked with the "final" boolean flag.
    _finalStates =
      Set.map ((,False) . ReadState) (SDPDA.finalStates pda)
        `Set.union` Set.map (,True) (Set.fromList universeF)
    -- Read transition function.
    _transRead =
      \case
        ((Start, _), a) ->
          let q = SDPDA.startState pda
           in -- From the start state, we only want to progress if a read state is
              -- reachable from the start state of the Sipser DPDA via ε-transitions.
              case nextState (q, []) of
                -- From here, we'd like to go directly to a pop state `(q', Just a, False)`,
                -- but this means we can't push `ts'` to the stack!
                -- Instead, we fold `stepPop` over the stack, starting from the read state
                -- (much like in `stepRead`).
                -- This ensures that either the entire stack is consumed by pop-steps,
                -- or we hit a new read state at some point,
                -- and so we can push the remaining stack.
                Left ((ReadState q', _), ts') ->
                  let f = stepPop fpda
                      p = Right (PopState q' (Just a), False)
                   in foldl f p ts'
                _ -> stuck False
        -- If we are in a stuck state and read any input symbol,
        -- we want to reject the word. We do this by staying in
        -- the stuck state, and setting the "final" flag to false.
        ((Stuck, _), _) -> stuck False
        -- Otherwise we move to a pop state, which also handles
        -- reading the input `a`.
        ((ReadState q, _), a) -> Right (PopState q (Just a), False)
    -- Pop transition function.
    _transPop =
      \case
        -- Case 1: in state `q` with input symbol `a` and stack symbol `t`.
        ((PopState q (Just a), _), SSymbol t) ->
          -- We know we are in a read state, and so one of
          -- δ(q, t, a) or δ(q, ε, a) must be defined in the Sipser DPDA.
          -- If δ(q, ε, a) is defined, we don't want to consume `t`,
          -- and so we pass `[t]` to `stepDPDA`, which will put `t` at
          -- the bottom of the stack after the δ transition.
          let readAndPop = stepDPDA [] (q, Just t, Just a)
              readOnly = stepDPDA [t] (q, Nothing, Just a)
           in case catMaybes [readAndPop, readOnly] of
                [Left ((r', b'), ts')] -> Left ((r', b'), ts')
                [Right (p', b')] -> Right (p', b')
                _ -> error "Malformed Sipser DPDA"
        -- Case 2: in state `q` with input symbol `a` and empty stack.
        ((PopState q (Just a), _), Bottom) ->
          -- We know we are in a read state, and so one of
          -- δ(q, t, a) or δ(q, ε, a) must be defined in the Sipser DPDA.
          case stepDPDA [] (q, Nothing, Just a) of
            -- If δ(q, t, a) is defined, then we are popping an empty stack,
            -- and so we are stuck.
            Nothing -> stuck False
            -- If δ(q, ε, a) is defined, and the result is a pop state,
            -- then we are also stuck as the stack is empty.
            Just (Right (_, b')) -> stuck b'
            -- If δ(q, ε, a) is defined, and the result is a read state,
            -- then we continue, remembering to add the `Bottom` symbol
            -- to the bottom of the stack.
            Just (Left ((r', b'), ts')) -> Left ((r', b'), ts' ++ [Bottom])
        -- Case 3: in state `q` with no input symbol and stack symbol `t`.
        ((PopState q Nothing, b), SSymbol t) ->
          -- We know we are in a pop state, and not a read state, and so
          -- δ(q, t, ε) must be defined in the Sipser DPDA.
          -- As we are not reading any new input, we propagate the "final" flag.
          case stepDPDA [] (q, Just t, Nothing) of
            Just (Left ((r', b'), ts')) -> Left ((r', b || b'), ts')
            Just (Right (p', b')) -> Right (p', b || b')
            Nothing -> error "Malformed Sipser DPDA"
        -- Case 4: in state `q` with no input symbol and empty stack.
        ((PopState _ Nothing, b), Bottom) ->
          -- We know we are in a pop state, and not a read state, and so
          -- δ(q, t, ε) must be defined in the Sipser DPDA.
          -- As the stack is empty, this means that we are stuck.
          stuck b
    -- Step the Sipser DPDA with configuration `(q, t, a)` and remaining stack `ts`,
    -- and then advance to the next reachable read- or pop state.
    stepDPDA :: [t] -> (s, Maybe t, Maybe a) -> Maybe (Either ((ReadState s, Bool), [Bottomed t]) (PopState s a, Bool))
    stepDPDA ts (q, t, a) = do
      (q', ts') <- SDPDA.trans pda (q, t, a)
      pure $ nextState (q', ts' ++ ts)
    -- Transitively advance to the next read- or pop state, from state `q` with stack `ts`.
    -- If we reach a read state, we also return the remaining stack symbols.
    -- If we reach a pop state, there can't be any remaining stack symbols,
    -- so we just return the state.
    -- In either case, the state is tagged with a boolean value,
    -- indicating that a final state was encountered along the way.
    nextState :: (s, [t]) -> Either ((ReadState s, Bool), [Bottomed t]) (PopState s a, Bool)
    nextState (q, ts) =
      case SDPDA.stepE pda [] (q, ts) of
        Right qs ->
          let b = (Set.fromList . toList) qs `intersects` SDPDA.finalStates pda
           in stuck b
        Left ((q', ts') :| cs)
          | isReadState q' ->
              let b = Set.fromList (q' : fmap fst cs) `intersects` SDPDA.finalStates pda
               in -- We step to the new state `q'`,
                  -- pushing the new stack symbols `ts''`,
                  -- and with the boolean flag `b`
                  -- signaling if we have reached a final state.
                  Left ((ReadState q', b), SSymbol <$> ts')
        Left ((q', []) :| cs)
          | isPopState q' ->
              let b = Set.fromList (q' : fmap fst cs) `intersects` SDPDA.finalStates pda
               in Right (PopState q' Nothing, b)
        Left _ -> error "Malformed Sipser DPDA"
    -- The stuck state is a read state which indicates that the Sipser PDA can't
    -- make any more meaningful transitions,
    -- which can happens either if popping from an empty stack,
    -- or reaching an infinite cycle where we read no input and don't shrink the stack.
    -- The boolean flag indicates if this stuck state should be a final state,
    -- which can e.g. happen if we enter an infinite loop containing a final state.
    stuck :: Bool -> Either ((ReadState s, Bool), [Bottomed t]) x
    stuck b = Left ((Stuck, b), [])
    -- Check if a final state is reachable from state `q` with stack symbols `ts`,
    -- but consuming no input.
    finalReachable (q, ts) =
      let reachable = toList $ either (fst <$>) id $ SDPDA.stepE pda [] (q, ts)
       in Set.fromList reachable `intersects` SDPDA.finalStates pda
    -- Check if a state in the Sipser DPDA is a read state.
    -- A state q is counted as a read state if either δ(q, ε, a) or δ(q, t, a)
    -- is defined for some arbitrary a ∈ Σ and t ∈ Γ.
    isReadState, isPopState :: s -> Bool
    isReadState q =
      isJust (SDPDA.trans pda (q, Nothing, Just arbitraryInputSymbol))
        || isJust (SDPDA.trans pda (q, Just arbitraryStackSymbol, Just arbitraryInputSymbol))
    -- Check if a state in the Sipser DPDA is a pop state.
    -- A state q is counted as a pop state if δ(q, t, ε) is defined
    -- for some arbitrary t ∈ Γ.
    isPopState q =
      isJust (SDPDA.trans pda (q, Just arbitraryStackSymbol, Nothing))
        || isJust (SDPDA.trans pda (q, Just arbitraryStackSymbol, Just arbitraryInputSymbol))
    arbitraryInputSymbol :: a
    arbitraryInputSymbol = head universeF
    arbitraryStackSymbol :: t
    arbitraryStackSymbol = head universeF
    -- Is the intersections of `xs` and `ys` non-empty?
    intersects xs ys = not $ xs `Set.disjoint` ys

{- Bibliography
 - ~~~~~~~~~~~~
 - [1]: T. D. Randløv, “Toward a Monadic Functional Machine Model for Computability and Complexity Theory: Finite and Pushdown Automata,” Master’s Thesis, University of Copenhagen, 2023.
-}
