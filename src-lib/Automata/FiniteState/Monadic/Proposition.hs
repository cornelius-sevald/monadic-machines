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
import qualified Data.Set as Set
import Data.Universe.Class (Finite (..))
import GHC.IsList

type PropositionFA a s = MonadicFA a Proposition s

accepts :: (Ord s) => PropositionFA a s -> [a] -> Bool
accepts m w = acceptance $ runMFA m w
  where
    acceptance = evaluate id

toAFA :: (Ord s) => PropositionFA a s -> AFA a s
toAFA m =
  AFA
    { AFA.start = start m,
      AFA.final = final m,
      AFA.trans = _trans
    }
  where
    _trans (q, a) u =
      let prop = trans m (q, a)
       in evaluate u prop

fromAFA :: forall a s. (Finite s, Ord s) => AFA a s -> PropositionFA a s
fromAFA afa =
  MonadicFA
    { start = AFA.start afa,
      final = AFA.final afa,
      trans = _trans
    }
  where
    _trans (q, a) =
      let g = AFA.trans afa
          dnf = fromList [clause u | u <- universeF, g (q, a) u]
       in fromDNF dnf
    lit :: (s -> Bool) -> s -> NF.Literal s
    lit f s = if f s then NF.Variable s else NF.Negation s
    clause :: (s -> Bool) -> [NF.Literal s]
    clause u = [lit u q | q <- universeF]
