{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- TODO: Test the implementation of the acceptance function.
-- TODO: Add functions for converting to- and from NFAs (and maybe also DFAs).
module Automata.AFA
  ( AFA (..),
    accepts,
    steps,
    fromDFA,
    fromNFA,
    toNFA,
  )
where

import Automata.DFA (DFA)
import qualified Automata.DFA as DFA
import Automata.NFA (NFA)
import qualified Automata.NFA as NFA
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Universe.Class (Finite (universeF))
import GHC.Generics (Generic)

-- | Compute the corresponding set of an indicator function.
indicate :: (Ord a, Finite a) => (a -> Bool) -> Set a
indicate f = Set.fromList $ [x | x <- universeF, f x]

-- | An alternating finite automaton is a 5-tuple
--  (Q, Σ, g, s_1, F), where
--
--    1. Q is a finite set called the *states*,
--    2. Σ is a finite set called the *alphabet*,
--    3. g : Q → Σ × P(Q) → {0,1} is the *transition function*,
--    4. s_1 ∈ Q is the *start state*, and
--    5. F ⊆ Q is the *set of final states*.
--
-- This is based on the definition from [1] and [2],
-- however we represent {0,1}^Q -- that is, the set of all mappings from Q into {0, 1} --
-- instead as the powerset of Q such that for u ∈ {0,1}^Q,
-- we define the corresponding r ∈ P(Q) as q ∈ r ⇔ u(q) = 1 for all q ∈ Q.
data AFA a s = AFA
  { -- | The start state s_1.
    start :: s,
    -- | The set of final states F.
    final :: Set s,
    -- | The transition function g.
    trans :: s -> (a, Set s) -> Bool
  }
  deriving (Generic)

-- | The transition function extended to a mapping of
-- Q into the set of all mappings of Σ* × P(Q) into {0, 1}.
--
-- Adapdet from [2].
steps :: (Show s, Ord s, Finite s) => AFA a s -> s -> ([a], Set s) -> Bool
steps afa q (w, u) = case w of
  [] -> q `Set.member` u
  (a : v) -> gq (a, indicate $ g (v, u))
  where
    gq = trans afa q
    g (x, r) u' = steps afa u' (x, r)

-- | Does the AFA accept the input string @xs@?
--
-- TODO: Test that this works with some examples.
accepts :: (Show s, Ord s, Finite s) => AFA a s -> [a] -> Bool
accepts m w = steps m (start m) (w, indicate f)
  where
    f q = q `Set.member` final m

-- | From an n-state AFA (Q, Σ, g, q_1, F), and assuming wlog. that q_0 ∉ Q,
-- construct an equivalent (2^n + 1)-state NFA (P(Q) ∪ {q_0}, Σ, δ, q_0, {F}).
--
-- The construction works as follows:
--
-- Let I : (A → {0,1}) → P(A) be defined as:
--
--   I(f) = { x | x <- A, f x },
--
-- that is, if f is the characteristic function of set X ⊆ A, then I(f) = X.
--
-- We then construct a (2^n)-state NNFA (P(Q), Σ, Δ, S, {F}) with no ε-transitions,
-- that is, an NFA with a non-deterministic start, as follows:
--
--   Δ(u, a) = { u' | u' ∈ P(Q), I(g'(a, u')) = u }
--   S = { u | u ∈ P(Q), q_1 ∈ u}
--
-- where g'(a, u') q = g(q)(a, u').
-- This construction is adapted from section 4 of [2].
--
-- To convert the NNFA to an NFA,
-- we then add an additional dedicated start state q_0,
-- and connect it with ε-transitions like so:
--
--   δ(u,   a) = Δ(u, a)
--   δ(u,   ε) = ∅
--   δ(q_0, a) = ∅
--   δ(q_0, ε) = S
toNFA :: (Show s, Ord s, Finite s) => AFA a s -> NFA a (Maybe (Set s))
toNFA afa = NFA.NFA {NFA.trans = _trans, NFA.start = _start, NFA.final = _final}
  where
    -- The set of starting states of the NNFA.
    starts = [Just u | u <- universeF, start afa `Set.member` u]
    -- The starting state of the NFA.
    _start = Nothing
    -- The final states of the NFA.
    _final = Set.singleton $ Just $ final afa
    -- The non-ε-transitions of the NFA.
    _trans (Just u, Just a) =
      let g' x q = trans afa q x
       in [Just u' | u' <- universeF, indicate (g' (a, u')) == u]
    -- We connect the start state of the NFA to all start states of the NNFA
    -- with ε-transitions.
    _trans (Nothing, Nothing) = starts
    _trans _ = []

-- | Convert an NFA M = (Q, Σ, δ, s_1, F)
-- to an equivalent AFA (Q, Σ, g, s_1, F'),
-- where g is defined as follows:
--
--   g(q)(a, u) = 1 ⇔ ∃p ∈ Δ(q, a). p ∈ u.
--
-- where Δ(q, a) is defined as the ε-closure of δ(q, a),
-- and F' are the prefinal states of M, that is,
-- the set of states from which F is reachable while consuming no input.
--
-- Adapted from [2].
fromNFA :: (Show s, Ord s, Finite s) => NFA a s -> AFA a s
fromNFA nfa = AFA {start = _start, final = _final, trans = _trans}
  where
    _start = NFA.start nfa
    _final = NFA.prefinal nfa
    _trans q (a, u) =
      let ps = NFA.step nfa q a
       in any (`Set.member` u) ps

-- | Convert a DFA (Q, Σ, δ, s_1, F) to an equivalent AFA (Q, Σ, g, s_1, F).
-- This is implemented similarly to `fromNFA`,
-- simply treating the DFA as an NFA which always has only one possible next state.
fromDFA :: (Show s, Ord s) => DFA a s -> AFA a s
fromDFA dfa = AFA {start = _start, final = _final, trans = _trans}
  where
    _start = DFA.start dfa
    _final = DFA.final dfa
    _trans q (a, u) = DFA.step dfa q a `Set.member` u

{- Bibliography
 - ~~~~~~~~~~~~
 - [1]: Chandra, Ashok K.; Kozen, Dexter C.; Stockmeyer, Larry J. (1981). "Alternation"
 - [2]: Fellah, Abdelaziz, Helmut Jürgensen, and Sheng Yu. "Constructions for alternating finite automata." International journal of computer mathematics 35.1-4 (1990): 117-132.
-}
