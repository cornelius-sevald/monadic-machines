{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TupleSections #-}

-- | Functional Deterministic Pushdown Automata
module Automata.PushDown.FPDA where

import Automata.PushDown.SipserDPDA (EOISipserDPDA, SipserDPDA (SipserDPDA))
import qualified Automata.PushDown.SipserDPDA as SDPDA
import Automata.PushDown.Util
import Control.Arrow (Arrow (first))
import Data.Set (Set)
import qualified Data.Set as Set

-- | A Functional Pushdown Automaton (FPDA),
-- is a ?-tuple (P, R, Σ, Γ, δ_read, δ_pop) where,
--
--   - P, R, Σ, Γ are finite sets, called the *pop states*,
--     *read states*, *input symbols* and *stack symbols*, resp.
--   - q_1 ∈ (P + R) is the *start state*, and
--   - F ⊆ (P + R) is the set of *accepting states*.
--   - δ_read : R × Σ → (P + R) × Γ^* is the *read transition function*, and
--   - δ_pop  : P × Γ → (P + R) is the *pop transition function*.
--
-- The notation Γ^* denotes the set {ε} ∪ Γ ∪ (Γ × Γ) ∪ ....
--
-- The read- and pop states, and, input- and stack alphabet
-- is implicitly given by the types `p`, `r`, `a`, `t` respectively.
data FPDA r p a t = FPDA
  { start :: r,
    final :: Set (Either r p),
    transRead :: (r, a) -> (Either r p, [t]),
    transPop :: (p, t) -> Either (r, [t]) p
  }

steps :: FPDA r p a t -> Either r p -> ([a], [t]) -> (Either r p, [a], [t])
steps fpda (Left r) (a : as, ts) =
  let δ_read = transRead fpda
      (s', ts') = δ_read (r, a)
   in steps fpda s' (as, ts' <> ts)
steps fpda (Right p) (as, t : ts) =
  let δ_pop = transPop fpda
      onL = first Left
      onR = first Right . (,[])
      (s', ts') = either onL onR $ δ_pop (p, t)
   in steps fpda s' (as, ts' <> ts)
steps _ s (as, ts) = (s, as, ts)

accepts :: (Ord r, Ord p) => FPDA r p a t -> [a] -> Bool
accepts fpda as =
  let (s, bs, _) = steps fpda (Left $ start fpda) (as, [])
   in null bs && s `Set.member` final fpda

-- | Convert a Sipser DPDA to a Functional PDA.
--
-- TODO: Implement
fromSipserDPDA :: SipserDPDA s a t -> FPDA s s a t
fromSipserDPDA pda = error "TODO: implement"

-- | Convert a Functional PDA to a Sipser Deterministic PDA.
--
-- TODO: Implement
toSipserDPDA :: FPDA r p a t -> SipserDPDA (r, p) a t
toSipserDPDA fpda = error "TODO: implement"

{- Bibliography
 - ~~~~~~~~~~~~
 - [1]: T. D. Randløv, “Toward a Monadic Functional Machine Model for Computability and Complexity Theory: Finite and Pushdown Automata,” Master’s Thesis, University of Copenhagen, 2023.
-}
