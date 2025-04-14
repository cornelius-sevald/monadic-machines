{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLists #-}

-- | Monadic pushdown automaton with the `List` monad.
-- Equivalent to a non-deterministic PDA.
module Automata.PushDown.Monadic.List where

import Automata.PushDown.Monadic
import Automata.PushDown.SipserNPDA (SipserNPDA (SipserNPDA))
import qualified Automata.PushDown.SipserNPDA as SNPDA
import Automata.PushDown.Util (Bottomed (..))
import Control.Applicative (Alternative (empty))
import qualified Data.Set as Set
import Data.Universe.Class (Finite (..))

type ListPDA r p a t = MonadicPDA [] r p a t

acceptsAngelig :: (Ord r) => ListPDA r p a t -> [a] -> Bool
acceptsAngelig m w = acceptance $ runMPDA m w
  where
    acceptance = or

acceptsDemonic :: (Ord r) => ListPDA r p a t -> [a] -> Bool
acceptsDemonic m w = acceptance $ runMPDA m w
  where
    acceptance = and

-- | Convert a List PDA to a Sipser NPDA.
--
-- See 'Automata.PushDown.FPDA.toSipserDPDA'
-- for a description on how it works.
toSipserNPDA :: (Ord r, Ord p, Ord t) => ListPDA r p a t -> SipserNPDA (Maybe (Either r p)) a t
toSipserNPDA m =
  SipserNPDA
    { SNPDA.startState = _startState,
      SNPDA.finalStates = _finalStates,
      SNPDA.trans = _trans
    }
  where
    -- Mark a state as a read state
    readState = Just . Left
    _startState = Nothing
    _finalStates = Set.map readState (finalStates m)
    _trans =
      Set.fromList . \case
        (Nothing, Nothing, Nothing) ->
          let q = readState $ startState m
              t = startSymbol m
           in pure (q, [t])
        (Just (Left r), Nothing, Just a) -> do
          (q, ts) <- collect <$> transRead m (r, a)
          pure (Just q, ts)
        (Just (Right p), Just t, Nothing) -> do
          (q, ts) <- collect <$> transPop m (p, t)
          pure (Just q, ts)
        (_, _, _) -> empty
    collect :: Either (r, [t]) p -> (Either r p, [t])
    collect (Left (r, ts)) = (Left r, ts)
    collect (Right p) = (Right p, [])

-- | Convert a Sipser NPDA to a List PDA.
--
-- See 'Automata.PushDown.FPDA.fromSipserDPDA'
-- for a description on how it works.
fromSipserNPDA ::
  (Finite s, Finite a, Finite t, Ord s, Eq t) =>
  SipserNPDA s a t ->
  ListPDA (Maybe s, Bool) (s, Maybe a, Bool) a (Bottomed t)
fromSipserNPDA pda = error "TODO: implement"
