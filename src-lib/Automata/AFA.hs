{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- TODO: Test the implementation of the acceptance function.
-- TODO: Add functions for converting to- and from NFAs (and maybe also DFAs).
module Automata.AFA where

import qualified Automata.Class
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Universe.Class (Finite (universeF))
import GHC.Generics (Generic)
import Test.QuickCheck (Arbitrary (..), CoArbitrary, Function, applyFun, genericShrink)

-- | An alternating finite automaton is a 5-tuple
--  ($Q, \Sigma, \delta, q_0, F$), where
--
--    1. $Q$ is a finite set called the *states*,
--    2. $\Sigma$ is a finite set called the *alphabet*,
--    3. $\delta : Q \rightarrow (\Sigma \times \mathcal{P}(Q) \rightarrow \{0,1\})$ is the transition function,
--    4. $q_0 \in Q$ is the *start state*, and
--    5. $F \subseteq Q$ is the *set of final states*. [@sipser_introduction_2013]
--
-- The states and alphabet is implicitly given by the type.
data AFA a s = AFA
  { -- | The transition function $\delta$.
    trans :: s -> (a, Set s) -> Bool,
    -- | The start state $q_0$.
    start :: s,
    -- | The set of final states $F$.
    final :: Set s
  }
  deriving (Generic)

-- | The inductive $H_i$ function defined in [1].
-- We use a set of states rather than bit-vectors.
--
-- Also not technically a step function.
--
-- [1]: Chandra, Ashok K.; Kozen, Dexter C.; Stockmeyer, Larry J. (1981). "Alternation"
stepH :: (Ord s, Finite s) => AFA a s -> s -> [a] -> Set s -> Bool
stepH _ q [] qs = q `Set.member` qs
stepH afa q (x : xs) qs =
  let rs = Set.filter (\r -> stepH afa r xs qs) $ Set.fromList universeF
      gi = trans afa q
   in gi (x, rs)

accepts :: (Ord s, Finite s) => AFA a s -> [a] -> Bool
accepts m xs = stepH m (start m) xs (final m)

instance (Ord s, Finite s) => Automata.Class.Acceptor AFA a s where
  accepts = accepts

instance (Ord s, Function a, Function s, Arbitrary s, CoArbitrary s, CoArbitrary a) => Arbitrary (AFA a s) where
  arbitrary = do
    start' <- arbitrary
    final' <- Set.fromList <$> arbitrary
    trans' <- applyFun <$> arbitrary
    pure $ AFA {start = start', final = final', trans = trans'}
  shrink = genericShrink
