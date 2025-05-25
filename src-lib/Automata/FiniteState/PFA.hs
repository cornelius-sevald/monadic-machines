{-# LANGUAGE ScopedTypeVariables #-}

-- | Probabilistic finite automata
module Automata.FiniteState.PFA where

import Data.Foldable
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Universe.Class (Finite (..))
import Data.Universe.Helpers (Natural, Tagged (..))
import Numeric.Matrix (Matrix, MatrixElement)
import qualified Numeric.Matrix as M
import Numeric.Probability.Distribution (T, (??))

data PFA prob a s = PFA
  { -- The start state q_1.
    start :: s,
    -- The set of final states F.
    final :: Set s,
    -- The transition function δ.
    trans :: (s, a) -> T prob s
  }

-- The transition matrix A(x).
step ::
  (Eq s, Finite s, MatrixElement prob) =>
  PFA prob a s ->
  a ->
  Matrix prob
step pfa x = M.fromList [[p j i x | i <- universeF] | j <- universeF]
  where
    p j i y = (== i) ?? trans pfa (j, y)

-- The global transition matrix A*(w).
steps ::
  forall a s prob.
  (Eq s, Finite s, MatrixElement prob) =>
  PFA prob a s ->
  [a] ->
  Matrix prob
steps pfa w =
  let as = map (step pfa) w
   in foldl' (*) (M.unit n) as
  where
    n = fromIntegral $ unTagged (cardinality :: Tagged s Natural)

accepts ::
  forall a s prob.
  (Eq s, Enum s, Finite s, Ord prob, MatrixElement prob) =>
  PFA prob a s ->
  prob ->
  [a] ->
  Bool
accepts pfa η w =
  let mat = steps pfa w
   in M.at (v * mat * f) (1, 1) > η
  where
    v = M.matrix (1, n) (\(_, j) -> ind (j == j0))
    f = M.matrix (n, 1) (\(i, _) -> ind (i `Set.member` is))
    n = fromIntegral $ unTagged (cardinality :: Tagged s Natural)
    j0 = (succ . fromEnum) $ start pfa
    is = Set.map (succ . fromEnum) $ final pfa
    ind :: Bool -> prob
    ind b = if b then 1 else 0
