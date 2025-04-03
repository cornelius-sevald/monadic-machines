-- | Monadic pushdown automaton with the `List` monad.
-- Equivalent to an (existential) non-deterministic PDA.
module Automata.PushDown.Monadic.List
  ( ListPDA,
    accepts,
    fromSNPDA,
    toSNPDA,
  )
where

import Automata.PushDown.Monadic
import Automata.PushDown.SipserNPDA (SipserNPDA (SipserNPDA))
import qualified Automata.PushDown.SipserNPDA as SNPDA

type ListPDA s a t = MonadicPDA [] s a t

accepts :: (Ord s, Ord a, Ord t) => ListPDA s a t -> [a] -> Bool
accepts m w = acceptance $ runMPDA m w
  where
    acceptance = or

-- | TODO: Implement & test that this works.
fromSNPDA :: SipserNPDA s a t -> ListPDA s a t
fromSNPDA m = undefined

-- | TODO: Implement & test that this works.
toSNPDA :: ListPDA s a t -> SipserNPDA s a t
toSNPDA m = undefined
