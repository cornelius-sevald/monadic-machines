-- | Monadic pushdown automaton with the `Identity` monad.
-- Equivalent to a deterministic PDA.
module Automata.PushDown.Monadic.Identity where

import Automata.PushDown.DPDA (DPDA)
import Automata.PushDown.FPDA (FPDA (FPDA))
import qualified Automata.PushDown.FPDA as FPDA
import Automata.PushDown.Monadic
import Automata.PushDown.Util
import Data.Functor.Identity
import Data.Universe.Class (Finite)

type IdentityPDA s a t = MonadicPDA Identity s a t

accepts :: (Ord r) => IdentityPDA r p a t -> [a] -> Bool
accepts m w = acceptance $ runMPDA m w
  where
    acceptance = runIdentity

fromFPDA :: FPDA r p a t -> IdentityPDA r p a t
fromFPDA fpda =
  MonadicPDA
    { startSymbol = FPDA.startSymbol fpda,
      startState = FPDA.startState fpda,
      finalStates = FPDA.finalStates fpda,
      transRead = Identity . FPDA.transRead fpda,
      transPop = Identity . FPDA.transPop fpda
    }

toFPDA :: IdentityPDA r p a t -> FPDA r p a t
toFPDA m =
  FPDA
    { FPDA.startSymbol = startSymbol m,
      FPDA.startState = startState m,
      FPDA.finalStates = finalStates m,
      FPDA.transRead = runIdentity . transRead m,
      FPDA.transPop = runIdentity . transPop m
    }

-- | See 'FPDA.toDPDA'.
toDPDA :: (Ord r, Ord p) => IdentityPDA r p a t -> DPDA (Maybe (Either r p)) a t
toDPDA = FPDA.toDPDA . toFPDA

-- | See 'FPDA.fromDPDA'.
fromDPDA ::
  (Finite s, Finite a, Finite t, Ord s, Eq t) =>
  DPDA s a t ->
  IdentityPDA (FPDA.ReadState s, Bool) (FPDA.PopState s a, Bool) a (Bottomed t)
fromDPDA = fromFPDA . FPDA.fromDPDA
