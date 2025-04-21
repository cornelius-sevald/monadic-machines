-- | Monadic automaton with the `ListList` (double-nested list) monad.
-- Equivalent to an alternating finite automaton,
-- with DNF acceptance.
module Automata.FiniteState.Monadic.ListList
  ( ListListFA,
    fromAFA,
    toAFA,
    acceptsDNF,
    acceptsCNF,
  )
where

import Automata.FiniteState.AFA (AFA (AFA))
import qualified Automata.FiniteState.AFA as AFA
import Automata.FiniteState.Monadic
import Control.Arrow ((***))
import Data.Bifoldable (bifold)
import Data.Containers.ListUtils (nubOrd)
import Data.List (partition)
import Data.ListList
import qualified Data.Set as Set
import Data.Universe.Class (Finite (..))
import GHC.IsList

type ListListFA a s = MonadicFA a ListList s

-- | Convert an AFA to a ListListFA.
--
-- As the ListListFA is only capable of representing the positive boolean formulas over the states,
-- we need to double the number of states to simulate negation,
-- such that @Left q@ corresponds to @q@ and @Right q@ corresponds to @¬q@.
--
-- As such, we make the final states { Left q | q in F } ∪ { Right q | q not in F }.
--
-- For the transition function, if we are in a 'Left' state,
-- we select all functions Q -> {0,1} that satisfy the AFA transition function,
-- and then, for each such function, partitions the inner list into 'Left' and 'Right' states.
-- If we are instead in a 'Right' state, we do as before, but select all functions Q -> {0, 1}
-- that do *not* satisfy the AFA transition function, reflecting that we are in a negated state.
fromAFA :: (Finite s, Ord s) => AFA a s -> ListListFA a (Either s s)
fromAFA afa = MonadicFA {start = _start, final = _final, trans = _trans}
  where
    _start = Left $ AFA.start afa
    _final =
      let finals = AFA.final afa
          nonfinals = Set.fromList universeF `Set.difference` AFA.final afa
       in Set.map Left finals <> Set.map Right nonfinals
    _trans (_q, a) =
      let q = either id id _q
          sided = case _q of Left _ -> id; Right _ -> not
          g = AFA.trans afa (q, a)
          us = filter (sided . g) universeF
       in fromList $ part <$> us
      where
        -- Partition the universe into Left and Right based on
        -- the indicator function `u`.
        part :: (Finite s, Ord s) => (s -> Bool) -> [Either s s]
        part u = bifold $ (map Left *** map Right) (partition u universeF)

-- | Convert a ListListFA to an AFA.
--
-- We simply interpret the double-nested list as a DNF,
-- and evaluate it using the indicator function of the AFA.
toAFA :: (Finite s, Ord s) => ListListFA a s -> AFA a s
toAFA m = AFA {AFA.start = _start, AFA.final = _final, AFA.trans = _trans}
  where
    _start = start m
    _final = final m
    _trans (q, a) u =
      let xss = trans m (q, a)
       in asDNF u xss

acceptsDNF :: (Ord s) => ListListFA a s -> [a] -> Bool
acceptsDNF m w = acceptance $ runMFA' m removeDups w
  where
    acceptance = asDNF id
    removeDups = fromList . nubOrd . map nubOrd . toList

acceptsCNF :: (Ord s) => ListListFA a s -> [a] -> Bool
acceptsCNF m w = acceptance $ runMFA' m removeDups w
  where
    acceptance = asCNF id
    removeDups = fromList . nubOrd . map nubOrd . toList
