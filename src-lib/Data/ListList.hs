{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}

module Data.ListList where

import Control.Applicative
import GHC.Generics (Generic)
import qualified GHC.IsList as IsList
import ListT (ListT (..))
import qualified ListT
import Test.QuickCheck.Arbitrary

newtype ListList a = ListList {getListList :: ListT [] a}
  deriving (Show, Eq, Ord, Generic)

toList :: ListList a -> [[a]]
toList (ListList xss) = ListT.toList xss

fromList :: [[a]] -> ListList a
fromList = ListList . ListT . map go
  where
    go [] = Nothing
    go (y : ys) = Just (y, ListT.fromFoldable ys)

instance Semigroup (ListList a) where
  ListList xss <> ListList yss = ListList (xss <> yss)

instance Monoid (ListList a) where
  mempty = ListList mempty

instance Functor ListList where
  fmap f (ListList xss) = ListList (fmap f xss)

instance Applicative ListList where
  pure x = ListList (pure x)
  liftA2 f (ListList xss) (ListList yss) = ListList (liftA2 f xss yss)

instance Monad ListList where
  ListList xss >>= f = ListList (xss >>= getListList . f)

instance Alternative ListList where
  empty = mempty
  (<|>) = mappend

instance Foldable ListList where
  foldMap f (ListList xss) = foldMap f xss

instance Traversable ListList where
  sequenceA (ListList xss) = ListList <$> sequenceA xss

instance IsList.IsList (ListList a) where
  type Item (ListList a) = [a]
  fromList = fromList
  toList = toList

instance (Arbitrary a) => Arbitrary (ListList a) where
  arbitrary = fromList <$> arbitrary
  shrink = (fromList <$>) . genericShrink . toList
