module Data.Alphabet where

import Data.Universe.Class
import Test.QuickCheck.Arbitrary

-- | An alphabet of two symbols.
data Bit = O | I
  deriving (Show, Eq, Ord, Enum, Bounded)

instance Universe Bit

instance Finite Bit

instance Arbitrary Bit where
  arbitrary = arbitraryBoundedEnum
  shrink O = []
  shrink I = [O]

-- | An alphabet of three symbols.
data ABC = A | B | C
  deriving (Show, Eq, Ord, Enum, Bounded)

instance Universe ABC

instance Finite ABC

instance Arbitrary ABC where
  arbitrary = arbitraryBoundedEnum
  shrink A = []
  shrink B = [A]
  shrink C = [A, B]
