{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TupleSections #-}

-- | Functional Deterministic Pushdown Automata
module Automata.PushDown.FPDA where

import Automata.PushDown.SipserDPDA (SipserDPDA (SipserDPDA))
import qualified Automata.PushDown.SipserDPDA as SDPDA
import Automata.PushDown.Util (Bottomed (..))
import Control.Monad (foldM)
import Data.Foldable (toList)
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
    transRead :: (r, a) -> p,
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
      s = Right $ transRead fpda (r, a)
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
-- TODO: Need to properly chain multiple pop states.
-- Currently, the only time a pop state moves to another
-- pop state is when moving to the "failure" state on an infinite
-- loop or popping an empty stack.
-- After moving from a pop state, I need to identify if the next state
-- is a pop- or read state, and I will also have to propagate if
-- any final states were reached.
fromSipserDPDA :: (Finite s, Ord s, Eq t) => SipserDPDA s a t -> FPDA (s, Bool) (Maybe (s, a)) a (Bottomed t)
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
      let q = SDPDA.startState pda
       in (q, finalReachable (q, []))
    _finalStates =
      Set.map (,False) (SDPDA.finalStates pda)
        `Set.union` Set.map (,True) (Set.fromList universeF)
    _transRead ((q, _), a) = Just (q, a)
    _transPop =
      \case
        -- In the "stuck" state, we stay until the stack is empty.
        (Nothing, _) -> Right Nothing
        -- In state `q` with input symbol `a` and stack symbol `t`.
        (Just (q, a), t) ->
          -- If we have reached the bottom of the stack,
          -- we step the Sipser DPDA with an empty stack,
          -- otherwise we step it with the top of the stack.
          let ts = case t of Bottom -> []; SSymbol t' -> [t']
           in case SDPDA.step pda a (q, ts) of
                -- Here we have reached an infinite loop or popped
                -- an empty stack *before* we consumed any input.
                -- In this case, we are stuck.
                Right _ -> Right Nothing
                -- Otherwise we reach state `q'` and pushed `ts'` to the stack.
                Left (q', ts') ->
                  -- If we are at the bottom of the stack,
                  -- we re-place the 'Bottom' symbol at the bottom of the stack.
                  let ts'' = fmap SSymbol ts' <> case t of Bottom -> [Bottom]; _ -> []
                      -- We check if any final states are reachable from here.
                      b = finalReachable (q', ts')
                   in -- We step to the new state `q'`,
                      -- pushing the new stack symbols `ts''`,
                      -- and with the boolean flag `b`
                      -- signaling if we have reached a final state.
                      Left ((q', b), ts'')
    -- Check if a final state (of the Sipser DPDA)
    -- is reachable from state `q` with no input and stack `ts`.
    finalReachable (q, ts) =
      let reachable = toList $ either (fst <$>) id $ SDPDA.stepE pda [] (q, ts)
       in not $ Set.fromList reachable `Set.disjoint` SDPDA.finalStates pda

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
    readState = Just . Left -- Mark a state as a read state
    popState = Just . Right -- Mark a state as a pop state
    _startState = Nothing
    _finalStates = Set.map readState (finalStates fpda)
    _trans = \case
      (Nothing, Nothing, Nothing) ->
        let q = readState $ startState fpda
            t = startSymbol fpda
         in Just (q, [t])
      (Just (Left r), Nothing, Just a) ->
        let q = popState $ transRead fpda (r, a)
         in Just (q, [])
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
