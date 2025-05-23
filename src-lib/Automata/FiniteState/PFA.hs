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
  { start :: s,
    final :: Set s,
    trans :: (s, a) -> T prob s
  }

step :: (Eq s, Finite s, MatrixElement prob) => PFA prob a s -> a -> Matrix prob
step pfa x = M.fromList [[p j i x | i <- universeF] | j <- universeF]
  where
    p j i y = (== i) ?? trans pfa (j, y)

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
  (Eq s, Enum s, Finite s, Ord prob, MatrixElement prob) =>
  PFA prob a s ->
  prob ->
  [a] ->
  Bool
accepts pfa η w =
  let i1 = (succ . fromEnum) $ start pfa
      js = Set.map (succ . fromEnum) $ final pfa
      mat = steps pfa w
      fs = M.select (\(i, j) -> i == i1 && j `Set.member` js) mat
   in sum fs > η
