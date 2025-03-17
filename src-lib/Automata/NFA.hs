{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | Non-deterministic finite automata
module Automata.NFA (NFA (..), closureE, step, fromDFA, toDFA) where

import qualified Automata.Class
import Automata.DFA (DFA)
import qualified Automata.DFA as DFA
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Universe.Class (Finite (universeF))
import GHC.Generics (Generic)
import Test.QuickCheck (Arbitrary (..), CoArbitrary, Function (function), applyFun, genericShrink)

-- | A non-deterministic finite automaton is a 5-tuple
--  ($Q, \Sigma, \delta, q_0, F$), where
--
--    1. $Q$ is a finite set called the *states*,
--    2. $\Sigma$ is a finite set called the *alphabet*,
--    3. $\delta : Q \times \Sigma_\epsilon \rightarrow \mathcal{P}(Q)$ is the transition function,
--    4. $q_0 \in Q$ is the *start state*, and
--    5. $F \subseteq Q$ is the *set of final states*. [@sipser_introduction_2013]
--
-- The states and alphabet is implicitly given by the type.
data NFA a s = NFA
  { -- | The start state $q_0$.
    start :: s,
    -- | The set of final states $F$.
    final :: Set s,
    -- | The transition function $\delta$.
    trans :: (s, Maybe a) -> [s]
  }
  deriving (Generic)

instance
  ( Show a,
    Show s,
    Function a,
    Function s
  ) =>
  Show (NFA a s)
  where
  show nfa =
    "NFA { start = "
      ++ show (start nfa)
      ++ ", "
      ++ "final = "
      ++ show (final nfa)
      ++ ", "
      ++ "trans = "
      ++ show trans'
      ++ " }"
    where
      trans' = function (trans nfa)

-- | Step the NFA in state @q@ with input symbol @x@.
step :: (Ord s) => NFA a s -> s -> a -> [s]
step nfa q x =
  case stepE nfa q of
    [] -> step1 q
    (r : rs) -> step1 q ++ step1 r ++ stepMany rs
  where
    step1 r = trans nfa (r, Just x)
    stepMany = concatMap (flip (step nfa) x)

-- | Does the NFA @m@ accept the input string @xs@?
accepts :: (Ord s) => NFA a s -> [a] -> Bool
accepts m xs = any (`Set.member` final m) r_n
  where
    r_0 = start m
    r_n = foldM (step m) r_0 xs

-- | Convert a DFA to an equivalent NFA.
fromDFA :: DFA a s -> NFA a s
fromDFA dfa =
  NFA
    { start = DFA.start dfa,
      final = DFA.final dfa,
      trans = delta
    }
  where
    delta (_, Nothing) = []
    delta (q, Just x) = [DFA.trans dfa (q, x)]

-- | [WIP] Convert a NFA to an equivalent DFA.
-- TODO: Also handle epsilon transitions.
toDFA :: (Ord s, Finite s) => NFA a s -> DFA a (Set s)
toDFA nfa =
  DFA.DFA
    { DFA.start = Set.singleton $ start nfa,
      DFA.final = fs,
      DFA.trans = delta
    }
  where
    qs = Set.powerSet $ Set.fromList universeF
    fs = Set.filter (any (`Set.member` final nfa)) qs
    delta (rs, x) =
      let d r = Set.fromList $ trans nfa (r, Just x)
       in Set.unions $ Set.map d rs

instance (Ord s) => Automata.Class.Acceptor NFA a s where
  accepts = accepts

instance (Ord s, Function a, Function s, Arbitrary s, CoArbitrary s, CoArbitrary a) => Arbitrary (NFA a s) where
  arbitrary = do
    start' <- arbitrary
    final' <- Set.fromList <$> arbitrary
    trans' <- applyFun <$> arbitrary
    pure $ NFA {start = start', final = final', trans = trans'}
  shrink = genericShrink
