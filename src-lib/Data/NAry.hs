{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Data.NAry
  ( NAry,
    ith,
    unIth,
    pattern Ith,
    relax,
    safeSucc,
    safePred,
    safePred',
  )
where

import Control.Arrow ((***))
import Data.Data (Proxy (Proxy))
import Data.Universe.Class
import Data.Universe.Helpers (Tagged (Tagged))
import GHC.TypeNats
import Test.QuickCheck (Function (..), functionBoundedEnum)
import Test.QuickCheck.Arbitrary

-- | An n-ary type, one that can inhabit exactly @n@ different values.
-- @NAry 1@ is equivalent to the unit type (with @Ith 1@ as its only inhabitant)
-- and @NAry 0@ is equivalent to @Void@.
newtype NAry (n :: Nat) = Ith# Nat
  deriving (Eq, Ord, Show)

instance (KnownNat n) => Enum (NAry n) where
  fromEnum (Ith# i) = fromIntegral i - 1

  toEnum i
    | i' < 0 = error "Data.NAry.Enum.toEnum: value is negative"
    | i' >= n' = error ("Data.NAry.toEnum: value is >= " ++ show n')
    | otherwise = Ith# (i' + 1)
    where
      i' = fromIntegral i
      n' = natVal (Proxy :: Proxy n)

instance (KnownNat n) => Bounded (NAry n) where
  minBound
    | n' == 0 = error "Data.NAry.Bounded.minBound: no minimum bound of 0-ary type."
    | otherwise = Ith# 1
    where
      n' = natVal (Proxy :: Proxy n)
  maxBound
    | n' == 0 = error "Data.NAry.Bounded.maxBound: no maximum bound of 0-ary type."
    | otherwise = Ith# n'
    where
      n' = natVal (Proxy :: Proxy n)

-- | Construct the @i@th value of this type.
-- It must be the case that @0 < i <= n@.
ith :: (Integral i, KnownNat n) => i -> NAry n
ith = toEnum . fromIntegral . pred

-- | Get the underlying number of this type,
-- i.e. if @i = unIth x@ then @x@ is the @i@th value of this type.
unIth :: (Integral i, KnownNat n) => NAry n -> i
unIth = succ . fromIntegral . fromEnum

pattern Ith :: Nat -> NAry n
pattern Ith i <- Ith# i

relax :: forall n m proxy. (KnownNat n, KnownNat m, n <= m) => NAry n -> proxy m -> NAry m
relax (Ith# i) _ = Ith# i

safeSucc :: (KnownNat n) => NAry n -> NAry (n + 1)
safeSucc (Ith# i) = Ith# (succ i)

safePred :: (KnownNat n) => NAry n -> Maybe (NAry (n - 1))
safePred (Ith# 1) = Nothing
safePred (Ith# i) = Just $ Ith# (i - 1)

safePred' :: NAry n -> Maybe (NAry n)
safePred' (Ith# 1) = Nothing
safePred' (Ith# i) = Just $ Ith# (i - 1)

-- | This is only for the 'fromInteger' function,
-- which will let us write n-ary types as integer literals
-- (however unsafe that might be).
instance (KnownNat n) => Num (NAry n) where
  fromInteger = ith
  abs = id
  signum n = case n of (Ith# 0) -> 0; (Ith# _) -> 1
  (+) = undefined
  (*) = undefined
  negate = undefined

instance (KnownNat n) => Real (NAry n) where
  toRational (Ith# i) = toRational i

instance (KnownNat n) => Integral (NAry n) where
  quotRem (Ith# i) (Ith# j) = (Ith# *** Ith#) (quotRem i j)
  toInteger (Ith# i) = toInteger i

instance (KnownNat n) => Universe (NAry n) where
  universe = ith <$> [1 .. n']
    where
      n' = natVal (Proxy :: Proxy n)

instance (KnownNat n) => Finite (NAry n) where
  cardinality = Tagged $ natVal (Proxy :: Proxy n)

instance (KnownNat n) => Arbitrary (NAry n) where
  arbitrary = arbitraryBoundedEnum
  shrink (Ith# i) = ith <$> [1 .. i - 1]

instance (KnownNat n) => Function (NAry n) where
  function = functionBoundedEnum

instance (KnownNat n) => CoArbitrary (NAry n) where
  coarbitrary = coarbitraryIntegral . (unIth :: NAry n -> Integer)
