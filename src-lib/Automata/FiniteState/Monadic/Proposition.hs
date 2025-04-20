-- | Monadic automaton with the `Proposition` monad.
-- Equivalent to an alternating finite automaton,
-- augmented with negation as well.
module Automata.FiniteState.Monadic.Proposition
  ( PropositionFA,
    fromAFA,
    toAFA,
    accepts,
  )
where

import Automata.FiniteState.AFA (AFA (AFA))
import qualified Automata.FiniteState.AFA as AFA
import Automata.FiniteState.Monadic
import Data.Logic.NormalForm (Literal (Variable))
import qualified Data.Logic.NormalForm as NF
import Data.Logic.Proposition
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Universe.Class (Finite (..))

type PropositionFA a s = MonadicFA a Proposition s

fromAFA :: (Finite s, Ord s) => AFA a s -> PropositionFA a s
fromAFA afa = MonadicFA {start = _start, final = _final, trans = _trans}
  where
    _start = AFA.start afa
    _final = AFA.final afa
    _trans (q, a) =
      let g u = AFA.trans afa q (a, u)
          dnf = Set.map complete $ Set.filter g (Set.fromList universeF)
          dnf' = NF.DNF $ NF.Disj $ Set.map NF.Conj dnf
       in fromDNF dnf'
      where
        complete :: (Finite a, Ord a) => Set a -> Set (NF.Literal a)
        complete s =
          let c = Set.fromList universeF `Set.difference` s
           in Set.map Variable s `Set.union` Set.map NF.Negation c

toAFA :: (Ord s) => PropositionFA a s -> AFA a s
toAFA m = AFA {AFA.start = _start, AFA.final = _final, AFA.trans = _trans}
  where
    _start = start m
    _final = final m
    _trans q (a, qs) =
      let prop = trans m (q, a)
       in evaluate (`Set.member` qs) prop

accepts :: (Ord s) => PropositionFA a s -> [a] -> Bool
accepts m w = acceptance $ runMFA m w
  where
    acceptance = evaluate id
