{-# LANGUAGE DeriveGeneric #-}

module Data.Alphabet where

import Data.Universe.Class
import GHC.Generics (Generic)
import Test.QuickCheck (Function)
import Test.QuickCheck.Arbitrary

-- | An alphabet of two symbols.
data Bit = O | I
  deriving (Show, Eq, Ord, Enum, Bounded, Generic)

instance Universe Bit

instance Finite Bit

instance Arbitrary Bit where
  arbitrary = arbitraryBoundedEnum
  shrink = genericShrink

instance CoArbitrary Bit where
  coarbitrary = genericCoarbitrary

instance Function Bit

-- | An alphabet of three symbols.
data ABC = A | B | C
  deriving (Show, Eq, Ord, Enum, Bounded, Generic)

instance Universe ABC

instance Finite ABC

instance Arbitrary ABC where
  arbitrary = arbitraryBoundedEnum
  shrink = genericShrink

instance CoArbitrary ABC where
  coarbitrary = genericCoarbitrary

instance Function ABC
