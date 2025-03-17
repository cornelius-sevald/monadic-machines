{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | Deterministic finite automata
module Automata.DFA (DFA (..), step) where

import qualified Automata.Class
import Data.Set (Set)
import qualified Data.Set as Set
import GHC.Generics
import Test.QuickCheck (Arbitrary (..), CoArbitrary, Function (function), applyFun, genericShrink)

-- | A deterministic finite automaton is a 5-tuple
--  ($Q, \Sigma, \delta, q_0, F$), where
--
--    1. $Q$ is a finite set called the *states*,
--    2. $\Sigma$ is a finite set called the *alphabet*,
--    3. $\delta : Q \times \Sigma \rightarrow Q$ is the transition function,
--    4. $q_0 \in Q$ is the *start state*, and
--    5. $F \subseteq Q$ is the *set of final states*. [@sipser_introduction_2013]
--
-- The states and alphabet is implicitly given by the type.
data DFA a s = DFA
  { -- | The start state $q_0$.
    start :: s,
    -- | The set of final states $F$.
    final :: Set s,
    -- | The transition function $\delta$.
    trans :: (s, a) -> s
  }
  deriving (Generic)

instance
  ( Show a,
    Show s,
    Function a,
    Function s
  ) =>
  Show (DFA a s)
  where
  show dfa =
    "DFA { start = "
      ++ show (start dfa)
      ++ ", "
      ++ "final = "
      ++ show (final dfa)
      ++ ", "
      ++ "trans = "
      ++ show trans'
      ++ " }"
    where
      trans' = function (trans dfa)

-- | Step the DFA in state @q@ with input symbol @x@.
step :: DFA a s -> s -> a -> s
step dfa q x = trans dfa (q, x)

-- | Does the DFA @m@ accept the input string @xs@?
accepts :: (Ord s) => DFA a s -> [a] -> Bool
accepts m xs = r_n `Set.member` final m
  where
    r_0 = start m
    r_n = foldl (step m) r_0 xs

instance (Ord s) => Automata.Class.Acceptor DFA a s where
  accepts = accepts

instance (Ord s, Function a, Function s, Arbitrary s, CoArbitrary s, CoArbitrary a) => Arbitrary (DFA a s) where
  arbitrary = do
    start' <- arbitrary
    final' <- Set.fromList <$> arbitrary
    trans' <- applyFun <$> arbitrary
    pure $ DFA {start = start', final = final', trans = trans'}
  shrink = genericShrink
