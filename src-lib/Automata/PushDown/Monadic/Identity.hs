-- | Monadic pushdown automaton with the `Identity` monad.
-- Equivalent to a deterministic PDA.
module Automata.PushDown.Monadic.Identity
  ( IdentityPDA,
    accepts,
    fromFPDA,
    toFPDA,
  )
where

import Automata.PushDown.FPDA (FPDA (FPDA))
import qualified Automata.PushDown.FPDA as FPDA
import Automata.PushDown.Monadic
import Data.Functor.Identity

type IdentityPDA s a t = MonadicPDA Identity s a t

accepts :: (Ord s, Ord a, Ord t) => IdentityPDA s a t -> [a] -> Bool
accepts m w = acceptance $ runMPDA m w
  where
    acceptance = runIdentity

-- | TODO: Implement & test that this works.
fromFPDA :: FPDA s a t -> IdentityPDA s a t
fromFPDA m = undefined

-- | TODO: Implement & test that this works.
toFPDA :: IdentityPDA s a t -> FPDA s a t
toFPDA m = undefined
