{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}

module Data.ListList where

import Control.Applicative
import GHC.Generics (Generic)
import qualified GHC.IsList as IsList
import Test.QuickCheck.Arbitrary

newtype ListList a = ListList {getListList :: [[a]]}
  deriving (Show, Eq, Ord, Generic)

toList :: ListList a -> [[a]]
toList (ListList xss) = xss

fromList :: [[a]] -> ListList a
fromList = ListList

instance Semigroup (ListList a) where
  ListList xss <> ListList yss = ListList (xss <> yss)

instance Monoid (ListList a) where
  mempty = ListList mempty

instance Functor ListList where
  fmap f (ListList xss) = ListList ((fmap . fmap) f xss)

instance Applicative ListList where
  pure x = ListList [[x]]
  liftA2 f (ListList xss) (ListList yss) =
    let gss = (fmap . fmap) f xss
     in ListList [gs <*> ys | gs <- gss, ys <- yss]

-- | WARNING: DO NOT USE THIS, IT DOES NOT RESPECT THE ASSOCIATIVITY LAW.
--
-- (and I can't figure out how to make it work)
instance Monad ListList where
  ListList xs >>= f =
    let ys = (fmap . fmap) (getListList . f) xs
        zs = concatMap (fmap concat . choices) ys
     in ListList zs

instance Alternative ListList where
  empty = mempty
  (<|>) = mappend

instance Foldable ListList where
  foldMap f (ListList xss) =
    let ys = fmap (foldMap f) xss
     in mconcat ys

instance Traversable ListList where
  sequenceA (ListList xss) =
    let yss = fmap sequenceA xss
        zss = sequenceA yss
     in fmap ListList zss

instance IsList.IsList (ListList a) where
  type Item (ListList a) = [a]
  fromList = fromList
  toList = toList

instance (Arbitrary a) => Arbitrary (ListList a) where
  arbitrary = fromList <$> arbitrary
  shrink = genericShrink

choices :: [[a]] -> [[a]]
choices = foldr (liftA2 (:)) [[]]
