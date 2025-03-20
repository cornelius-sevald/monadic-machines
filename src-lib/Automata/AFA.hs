{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- TODO: Test the implementation of the acceptance function.
-- TODO: Add functions for converting to- and from NFAs (and maybe also DFAs).
module Automata.AFA
  ( AFA (..),
    _H,
    _G,
    stepG,
    fromDFA,
    fromNFA,
    toNFA,
  )
where

import qualified Automata.Class
import Automata.DFA (DFA)
import qualified Automata.DFA as DFA
import Automata.NFA (NFA)
import qualified Automata.NFA as NFA
import Data.Sequence (Seq (..))
import qualified Data.Sequence as Seq
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Universe.Class (Finite (universeF))
import GHC.Generics (Generic)

-- | An alternating finite automaton is a 5-tuple
--  (Q, Σ, δ, q_0, F), where
--
--    1. Q is a finite set called the *states*,
--    2. Σ is a finite set called the *alphabet*,
--    3. g : Q → Σ × P(Q) → {0,1} is the *transition function*,
--    4. q_0 ∈ Q is the *start state*, and
--    5. F ⊆ Q is the *set of final states*. [1]
--
-- The states and alphabet is implicitly given by the type.
data AFA a s = AFA
  { -- | The start state q_0.
    start :: s,
    -- | The set of final states F.
    final :: Set s,
    -- | The transition function g.
    trans :: s -> (a, Set s) -> Bool
  }
  deriving (Generic)

-- | The inductive H function defined in [1].
-- We use a set of states rather than bit-vectors.
_H :: (Ord s, Finite s) => AFA a s -> s -> Seq a -> Set s -> Bool
_H _ i Empty u = i `Set.member` u
_H afa i (a :<| x) u =
  let g = trans afa
      hs = Set.filter (\j -> _H afa j x u) $ Set.fromList universeF
   in g i (a, hs)

stepG :: (Ord s, Finite s) => AFA a s -> a -> Set s -> Set s
stepG afa a u =
  let g = trans afa
   in Set.filter (\j -> g j (a, u)) $ Set.fromList universeF

-- | The inductive G function defined in [1].
-- We use a set of states rather than bit-vectors.
_G :: (Ord s, Finite s) => AFA a s -> s -> Seq a -> Set s -> Bool
_G _ i Empty u = i `Set.member` u
_G afa i (x :|> a) u = _G afa i x $ stepG afa a u

-- | Does the AFA accept the input string @xs@?
-- TODO: Test that this works.
accepts :: (Ord s, Finite s) => AFA a s -> [a] -> Bool
accepts m xs =
  let xs' = Seq.fromList xs
   in _H m (start m) xs' (final m)

-- | To convert a NFA to a DFA, first assume that the NFA has no ε-transitions.
-- Then the transition function $g$ of the AFA for state $q$ and input symbol $x$
-- is true iff. the input set of states $rs$ is the result of the transition function $δ$ of the NFA, i.e.
--
-- >  g(q)(x, rs) ⇔ δ(q, x) = rs.
--
-- To account for the ε-transitions, instead of using δ directly we use the ε-closure of the transition function.
fromNFA :: (Ord s, Finite s) => NFA a s -> AFA a s
fromNFA nfa = AFA {start = _start, final = _final, trans = _trans}
  where
    _start = NFA.start nfa
    _final = prefinal
    _trans q (a, u) =
      let r = NFA.step nfa q a
       in intersects r u
    -- All states that can reach the final state with no input.
    prefinal = Set.fromList [q | q <- universeF, intersects (NFA.closureE nfa q) (NFA.final nfa)]
    intersects r u = not $ Set.null $ Set.intersection r u

-- | To convert a DFA to an AFA,
-- we let the transition function $g$ of the AFA for state $q$ and input symbol $x$
-- be true iff. the input set of states $rs$ is a singleton $r$, which is the result
-- of the transition function δ of the DFA, i.e.
--
-- >  g(q)(x, rs) ⇔ rs = { r } ∧ δ(q, x) = r.
fromDFA :: (Ord s) => DFA a s -> AFA a s
fromDFA dfa = AFA {start = _start, final = _final, trans = _trans}
  where
    _start = DFA.start dfa
    _final = DFA.final dfa
    _trans q (x, rs) = DFA.step dfa q x `Set.member` rs

-- | To convert an AFA (Q, Σ, s, F, g)
-- to an NFA (P(Q), Σ, s', F, δ),
-- we first construct an NNFA -- a non-determinstic starting-state NFA, i.e. an NFA with a set of starting states --
-- with the transition function defined as follows:
--
-- >  δ(qs, x) = rs ⇔ g(q, x, rs)
--
-- for all $q ∈ qs$ and $rs ∈ P(Q)$,
-- and set of starting states:
--
-- >  S = { qs | s ∈ qs }.[2]
--
-- To convert this NNFA to an equvalent NFA,
-- we let $\{ s \}$ be the starting state and create a ε-transition
-- from $\{ s \}$ to each $q ∈ S$.
--
-- TODO: FIX THIS
toNFA :: (Ord s, Finite s) => AFA a s -> NFA a (Set s)
toNFA afa = NFA.NFA {NFA.trans = _trans, NFA.start = _start, NFA.final = _final}
  where
    -- The set of starting states of the NNFA.
    starts = [qs | qs <- universeF, start afa `Set.member` qs]
    -- The starting state of the NFA, just a singleton of the AFA starting state.
    _start = Set.singleton (start afa)
    -- The final states of the NFA, just a singleton of the AFA final states.
    _final = Set.singleton (final afa)
    -- The non-ε-transitions of the NFA, as defined above.
    _trans (qs, Just x) = [rs | rs <- universeF, q <- Set.toList qs, trans afa q (x, rs)]
    -- We connect the start state of the NFA to all start states of the NNFA
    -- with ε-transitions.
    _trans (qs, Nothing)
      | qs == _start = starts
      | otherwise = []

-- TODO: Maybe write a dedicated function for this rather than just using `NFA.toDFA`.
-- TODO: Test that this works
-- toDFA :: (Ord s, Finite s) => AFA a s -> DFA a (Set (Set s))
-- toDFA = NFA.toDFA . toNFA

instance (Ord s, Finite s) => Automata.Class.Acceptor AFA a s where
  accepts = accepts

{- Bibliography
 - ~~~~~~~~~~~~
 - [1]: Chandra, Ashok K.; Kozen, Dexter C.; Stockmeyer, Larry J. (1981). "Alternation"
 - [2]: Fellah, Abdelaziz, Helmut Jürgensen, and Sheng Yu. "Constructions for alternating finite automata." International journal of computer mathematics 35.1-4 (1990): 117-132.
-}
