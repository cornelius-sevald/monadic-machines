{-# LANGUAGE ScopedTypeVariables #-}

-- | Monadic finite-state automaton with the `Proposition` monad.
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
import qualified Data.Logic.NormalForm as NF
import Data.Logic.Proposition
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Universe.Class (Finite (..))

type PropositionFA a s = MonadicFA a Proposition s

accepts :: (Ord s) => PropositionFA a s -> [a] -> Bool
accepts m w = acceptance $ runMFA m w
  where
    acceptance = evaluate id

fromAFA :: forall a s. (Finite s, Ord s) => AFA a s -> PropositionFA a s
fromAFA afa = MonadicFA {start = _start, final = _final, trans = _trans}
  where
    _start = AFA.start afa
    _final = AFA.final afa
    _trans (q, a) =
      let g = AFA.trans afa (q, a)
          us = filter g universeF
          dnf = Set.fromList $ lits <$> us
          dnf' = NF.DNF $ NF.Disj $ Set.map NF.Conj dnf
       in fromDNF dnf'
      where
        lit :: (s -> Bool) -> s -> NF.Literal s
        lit f s = if f s then NF.Variable s else NF.Negation s
        lits :: (s -> Bool) -> Set (NF.Literal s)
        lits u = Set.map (lit u) (Set.fromList universeF)

toAFA :: (Ord s) => PropositionFA a s -> AFA a s
toAFA m = AFA {AFA.start = _start, AFA.final = _final, AFA.trans = _trans}
  where
    _start = start m
    _final = final m
    _trans (q, a) u =
      let prop = trans m (q, a)
       in evaluate u prop
