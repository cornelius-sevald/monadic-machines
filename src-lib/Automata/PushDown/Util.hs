{-# LANGUAGE DeriveGeneric #-}

module Automata.PushDown.Util where

import Data.Bool (bool)
import Data.Foldable (find)
import Data.Maybe (listToMaybe)
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

{- These types are used for conversion of FPDAs to Sipser DPDAs. -}

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

data State s
  = Start
  | Middle s
  | Final
  deriving (Show, Eq, Ord, Generic)

instance (Arbitrary s) => Arbitrary (State s) where
  arbitrary = either (bool Start Final) Middle <$> arbitrary
  shrink = genericShrink
