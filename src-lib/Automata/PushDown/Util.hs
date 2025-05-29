{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Automata.PushDown.Util where

import Data.Foldable (find)
import Data.Maybe (listToMaybe)
import Data.Universe.Class
import Data.Universe.Helpers
import GHC.Generics (Generic)
import Test.QuickCheck

-- | Check if we have been in a similar configuration before.
-- Specifically, check if we have been in a configuration
-- with the same state, same top of the stack, and not a smaller stack.
--
-- This is equivalent to the `existsIn` function from [2].
dejavu :: (Eq s, Eq t, Foldable f) => f (s, [t]) -> (s, [t]) -> Maybe (s, [t])
dejavu before now = find (been now) before
  where
    been (s, ts) (q, ys) =
      -- States match,
      s == q
        -- and the top of the stacks match,
        && listToMaybe ts == listToMaybe ys
        -- and the `now` stack has not shrunk.
        && length ts >= length ys

-- | Split a list at the first index,
-- returning the head and tail of the list.
-- If the list is empty, the head is 'Nothing' and the tail is @[]@.
split1 :: [x] -> (Maybe x, [x])
split1 [] = (Nothing, [])
split1 (x : xs) = (Just x, xs)

-- | Split a list at the first *and* zeroth index,
-- returning both the head and tail of the list (as a pair),
-- and the entire list @xs@ (as a pair @('Nothing', xs)@).
--
-- If the list is empty, this is the same as @['split1' xs]@.
split01 :: [x] -> [(Maybe x, [x])]
split01 [] = [(Nothing, [])]
split01 (x : xs) = [(Nothing, x : xs), (Just x, xs)]

{- Types convenient for making PDAs -}

-- An input symbol, or an end-of-input marker.
data Ended a
  = ISymbol a
  | End
  deriving (Show, Eq, Ord, Generic)

end :: [a] -> [Ended a]
end w = fmap ISymbol w <> [End]

instance (Arbitrary a) => Arbitrary (Ended a) where
  arbitrary = maybe End ISymbol <$> arbitrary
  shrink = genericShrink

instance (Universe a) => Universe (Ended a) where
  universe = End : (ISymbol <$> universe)

instance (Finite a) => Finite (Ended a) where
  universeF = End : (ISymbol <$> universeF)
  cardinality = fmap succ (retag (cardinality :: Tagged a Natural))

-- A stack symbol, or a bottom-of-stack marker
data Bottomed a
  = SSymbol a
  | Bottom
  deriving (Show, Eq, Ord, Generic)

bottom :: [a] -> [Bottomed a]
bottom w = fmap SSymbol w <> [Bottom]

instance (Arbitrary a) => Arbitrary (Bottomed a) where
  arbitrary = maybe Bottom SSymbol <$> arbitrary
  shrink = genericShrink

instance (Universe a) => Universe (Bottomed a) where
  universe = Bottom : (SSymbol <$> universe)

instance (Finite a) => Finite (Bottomed a) where
  universeF = Bottom : (SSymbol <$> universeF)
  cardinality = fmap succ (retag (cardinality :: Tagged a Natural))
