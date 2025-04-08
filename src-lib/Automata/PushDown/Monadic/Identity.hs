-- | Monadic pushdown automaton with the `Identity` monad.
-- Equivalent to a deterministic PDA.
module Automata.PushDown.Monadic.Identity where

import Automata.PushDown.FPDA (FPDA (FPDA))
import qualified Automata.PushDown.FPDA as FPDA
import Automata.PushDown.Monadic
import Automata.PushDown.SipserDPDA (SipserDPDA)
import Data.Functor.Identity

type IdentityPDA s a t = MonadicPDA Identity s a t

accepts :: (Ord s, Ord a, Ord t) => IdentityPDA s a t -> [a] -> Bool
accepts m w = acceptance $ runMPDA m w
  where
    acceptance = runIdentity

fromFPDA :: FPDA s a t -> IdentityPDA s a t
fromFPDA fpda =
  MonadicPDA
    { start = FPDA.start fpda,
      final = FPDA.final fpda,
      startSymbol = FPDA.startSymbol fpda,
      transInput = Identity . FPDA.transInput fpda,
      transStack = Identity . FPDA.transStack fpda
    }

toFPDA :: IdentityPDA s a t -> FPDA s a t
toFPDA m =
  FPDA
    { FPDA.start = start m,
      FPDA.final = final m,
      FPDA.startSymbol = startSymbol m,
      FPDA.transInput = runIdentity . transInput m,
      FPDA.transStack = runIdentity . transStack m
    }

-- | See 'FPDA.fromSipserDPDA.
fromSipserDPDA :: (Ord s, Eq t) => SipserDPDA s a t -> IdentityPDA s a (Maybe t)
fromSipserDPDA = fromFPDA . FPDA.fromSipserDPDA

-- | See 'FPDA.toSipserDPDA'.
toSipserDPDA :: (Ord s, Ord a) => IdentityPDA s a t -> SipserDPDA (Either Bool (s, Maybe a)) (Maybe a) (Maybe t)
toSipserDPDA = FPDA.toSipserDPDA . toFPDA

-- | Wrapper acceptance function for Sipser DPDAs created via 'toSipserDPDA'.
-- Automatically ends an input word with a dedicated end-of-input symbol: 'Nothing'.
--
-- The signature of this function is needlessly specific (e.g. the type of the state could just be @s@),
-- but this is to prevent accidental misuse.
acceptsSipserDPDA :: (Ord s, Ord a, Eq t) => SipserDPDA (Either Bool (s, Maybe a)) (Maybe a) (Maybe t) -> [a] -> Bool
acceptsSipserDPDA = FPDA.acceptsSipserDPDA
