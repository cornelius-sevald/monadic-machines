-- | Monadic pushdown automaton with the `Identity` monad.
-- Equivalent to a deterministic PDA.
module Automata.PushDown.Monadic.Identity where

import Automata.PushDown.FPDA (FPDA (FPDA))
import qualified Automata.PushDown.FPDA as FPDA
import Automata.PushDown.Monadic
import Automata.PushDown.SipserDPDA (EOISipserDPDA, SipserDPDA)
import Automata.PushDown.Util
import Data.Functor.Identity

type IdentityPDA s a t = MonadicPDA Identity s a t

accepts :: (Ord s, Ord a, Ord t) => IdentityPDA s a t -> [a] -> Bool
accepts m w = acceptance $ runMPDA m w
  where
    acceptance = runIdentity

fromFPDA :: FPDA r p a t -> IdentityPDA s a t
fromFPDA fpda = undefined

toFPDA :: IdentityPDA s a t -> FPDA r p a t
toFPDA m = undefined

-- | See 'FPDA.fromSipserDPDA'.
fromSipserDPDA :: (Ord s, Eq t) => SipserDPDA s a t -> IdentityPDA s a t
fromSipserDPDA = error "TODO: implement"

-- | See 'FPDA.toSipserDPDA'.
toSipserDPDA :: (Ord s, Ord a) => IdentityPDA s a t -> EOISipserDPDA s a t
toSipserDPDA = error "TODO: implement"
