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
import Data.Maybe (isJust)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Universe.Class (Finite (universeF))

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

stepStack :: FPDA r p a t -> Either (r, [t]) p -> t -> Either (r, [t]) p
stepStack _ (Left (r, ts)) t = Left (r, ts <> [t])
stepStack fpda (Right p) t = transPop fpda (p, t)

step :: FPDA r p a t -> (r, [t]) -> a -> Maybe (r, [t])
step fpda (r, ts) a =
  let f = stepStack fpda
      s = transRead fpda (r, a)
   in catch $ foldl f s ts

steps :: FPDA r p a t -> [a] -> (r, [t]) -> Maybe (r, [t])
steps fpda as c = foldM (step fpda) c as

accepts :: (Ord r) => FPDA r p a t -> [a] -> Bool
accepts fpda as =
  let c_0 = (startState fpda, [startSymbol fpda])
   in case steps fpda as c_0 of
        Nothing -> False
        Just (q, _) -> q `Set.member` finalStates fpda

-- | Convert a Sipser DPDA to a Functional PDA.
--
-- TODO: Explain the inner machinations of this beast.
fromSipserDPDA ::
  forall s a t.
  (Finite s, Finite a, Finite t, Ord s, Eq t) =>
  SipserDPDA s a t ->
  FPDA (Maybe s, Bool) (s, Maybe a, Bool) a (Bottomed t)
fromSipserDPDA pda =
  FPDA
    { startSymbol = _startSymbol,
      startState = _startState,
      finalStates = _finalStates,
      transRead = _transRead,
      transPop = _transPop
    }
  where
    _startSymbol = Bottom
    _startState =
      -- If a final state is reachable from the start state
      -- without consuming any input, we want to mark it as so.
      let q = SDPDA.startState pda
       in (Just q, finalReachable (q, []))
    -- The final states of the FPDA is the union of the final states
    -- of the Sipser DPDA, and *all* of the states of the DPDA
    -- marked with the "final" boolean flag.
    _finalStates =
      Set.map ((,False) . Just) (SDPDA.finalStates pda)
        `Set.union` Set.map (,True) (Set.fromList universeF)
    -- Read transition function.
    _transRead =
      \case
        -- If we are in a stuck state and read any input symbol,
        -- we want to reject the word. We do this by staying in
        -- the stuck state, and setting the "final" flag to false.
        ((Nothing, _), _) -> stuck False
        -- Otherwise we move to a pop state, which also handles
        -- reading the input `a`.
        ((Just q, _), a) -> Right (q, Just a, False)
    -- Pop transition function.
    _transPop =
      \case
        -- Case 1: in state `q` with input symbol `a` and stack symbol `t`.
        ((q, Just a, _), SSymbol t) ->
          case SDPDA.step pda a (q, [t]) of
            -- Here we have reached an infinite loop or popped
            -- an empty stack *before* we consumed any input.
            -- In this case, we are stuck.
            Right _ -> stuck False
            -- Otherwise we have consumed the input symbol `a`,
            -- which have put us in state `q'` and pushed `ts'` to the stack.
            -- From there, we find the next push- or pop state reachable from `ts'`.
            Left (q', ts') -> case nextState (q', ts') of
              Left ((q'', b), ts'') -> Left ((q'', b), ts'')
              Right (q'', b) -> Right (q'', Nothing, b)
        -- Case 2: in state `q` with input symbol `a` and empty stack.
        ((q, Just a, _), Bottom) ->
          case SDPDA.step pda a (q, []) of
            -- Here we have reached an infinite loop or popped
            -- an empty stack *before* we consumed any input.
            -- In this case, we are stuck.
            Right _ -> stuck False
            -- Otherwise we have consumed the input symbol `a`,
            -- which have put us in state `q'` and pushed `ts'` to the stack.
            -- From there, we transitively step using only `ts'`.
            Left (q', ts') -> case nextState (q', ts') of
              -- Either we end up in a read state `q''` after pushing `ts''` to the stack.
              -- We then move to the read state, and remember to put the
              -- 'Bottom' symbol at the bottom of the stack again.
              Left ((q'', b), ts'') -> Left ((q'', b), ts'' ++ [Bottom])
              -- Or we end up in a pop state.
              -- In this case we are stuck, as the stack is empty.
              Right (_, b) -> stuck b
        -- Case 3: in state `q` with no input symbol and stack symbol `t`.
        ((q, Nothing, b), SSymbol t) ->
          -- We transitively step using only `t` on the stack.
          case nextState (q, [t]) of
            -- Either we end up in a read state `q'` after pushing `ts'` to the stack.
            Left ((q', b'), ts') -> Left ((q', b || b'), ts')
            -- Or we end up in a pop state.
            Right (q', b') -> Right (q', Nothing, b || b')
        -- Case 4: in state `q` with no input symbol and empty stack.
        ((q, Nothing, b), Bottom) ->
          case nextState (q, []) of
            -- Either we end up in a read state `q'` after pushing `ts'` to the stack.
            -- We then move to the read state, and remember to put the
            -- 'Bottom' symbol at the bottom of the stack again.
            Left ((q', b'), ts') -> Left ((q', b || b'), ts' ++ [Bottom])
            -- Or we end up in a pop state.
            -- In this case we are stuck, as the stack is empty.
            Right (_, b') -> stuck (b || b')
    -- Transitively advance to the next read- or pop state, from state `q` with stack `ts`.
    -- If we reach a read state, we also return the remaining stack symbols.
    -- If we reach a pop state, there can't be any remaining stack symbols,
    -- so we just return the state.
    -- In either case, the state is tagged with a boolean value,
    -- indicating that a final state was encountered along the way.
    nextState :: (s, [t]) -> Either ((Maybe s, Bool), [Bottomed t]) (s, Bool)
    nextState (q, ts) =
      case SDPDA.stepE pda [] (q, ts) of
        Right qs ->
          let b = (Set.fromList . toList) qs `intersects` SDPDA.finalStates pda
           in stuck b
        Left ((q', []) :| cs)
          | popState q' ->
              let b = Set.fromList (q' : fmap fst cs) `intersects` SDPDA.finalStates pda
               in Right (q', b)
        Left ((q', ts') :| cs)
          | readState q' ->
              -- If we are at the bottom of the stack,
              -- we re-place the 'Bottom' symbol at the bottom of the stack.
              let ts'' = fmap SSymbol ts'
                  b = Set.fromList (q' : fmap fst cs) `intersects` SDPDA.finalStates pda
               in -- We step to the new state `q'`,
                  -- pushing the new stack symbols `ts''`,
                  -- and with the boolean flag `b`
                  -- signaling if we have reached a final state.
                  Left ((Just q', b), ts'')
        Left _ -> error "Malformed Sipser DPDA"
    -- The stuck state is a read state which indicates that the Sipser PDA can't
    -- make any more meaningful transitions,
    -- which can happens either if popping from an empty stack,
    -- or reaching an infinite cycle where we read no input and don't shrink the stack.
    -- The boolean flag indicates if this stuck state should be a final state,
    -- which can e.g. happen if we enter an infinite loop containing a final state.
    stuck :: Bool -> Either ((Maybe s, Bool), [Bottomed t]) x
    stuck b = Left ((Nothing, b), [])
    -- Check if a final state is reachable from state `q` with stack symbols `ts`,
    -- but consuming no input.
    finalReachable (q, ts) =
      let reachable = toList $ either (fst <$>) id $ SDPDA.stepE pda [] (q, ts)
       in Set.fromList reachable `intersects` SDPDA.finalStates pda
    -- Check if a state in the Sipser DPDA is a read state.
    -- A state q is counted as a read state if either δ(q, ε, a) or δ(q, t, a)
    -- is defined for some arbitrary a ∈ Σ and t ∈ Γ.
    readState, popState :: s -> Bool
    readState q =
      isJust (SDPDA.trans pda (q, Nothing, Just arbitraryInputSymbol))
        || isJust (SDPDA.trans pda (q, Just arbitraryStackSymbol, Just arbitraryInputSymbol))
    -- Check if a state in the Sipser DPDA is a pop state.
    -- A state q is counted as a pop state if δ(q, t, ε) is defined
    -- for some arbitrary t ∈ Γ.
    popState q =
      isJust (SDPDA.trans pda (q, Just arbitraryStackSymbol, Nothing))
    arbitraryInputSymbol :: a
    arbitraryInputSymbol = head universeF
    arbitraryStackSymbol :: t
    arbitraryStackSymbol = head universeF
    -- Is the intersections of `xs` and `ys` non-empty?
    intersects xs ys = not $ xs `Set.disjoint` ys

-- | Convert a Functional PDA to a Sipser Deterministic PDA.
--
-- We let the states be Q = (R + P) ∪ {q₀}, where q₀ is a designated start state.
-- From q₀ we put the start symbol Z₀ on the stack, and then move to the
-- start state of the FPDA, i.e. we define δ(q₀, ε, ε) = (r₀, [Z₀]).
-- For a read state r, we define δ(r, ε, a) = (δ_read(r, a), ε).
-- For a pop state p, we define
-- δ(p, t, ε) = (δ_pop(p, t), [])     if δ_pop(p, t) is a pop state,
--            =  δ_pop(p, t)          otherwise
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
        let (q, ts) = case transRead fpda (r, a) of
              Left (r', ts') -> (Left r', ts')
              Right p' -> (Right p', [])
         in Just (Just q, ts)
      (Just (Right p), Just t, Nothing) ->
        let (q, ts) = case transPop fpda (p, t) of
              Left (r', ts') -> (Left r', ts')
              Right p' -> (Right p', [])
         in Just (Just q, ts)
      (_, _, _) -> Nothing

{- Bibliography
 - ~~~~~~~~~~~~
 - [1]: T. D. Randløv, “Toward a Monadic Functional Machine Model for Computability and Complexity Theory: Finite and Pushdown Automata,” Master’s Thesis, University of Copenhagen, 2023.
-}
