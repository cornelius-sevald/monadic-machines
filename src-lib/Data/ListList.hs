{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}

module Data.ListList
  ( ListList (..),
    toSet,
    fromSet,
    asCNF,
    asDNF,
  )
where

import Control.Applicative
import Data.Function (on)
import Data.Set (Set)
import qualified Data.Set as Set
import GHC.Generics (Generic)
import GHC.IsList
import Test.QuickCheck.Arbitrary

-- | A type of list-of-lists (or more accurately a set-of-sets).
--
-- This can be used to represent poitive CNF formulas,
-- and should work accordingly with the `Monad` instance.
--
-- NOTE: When doing comparisons, this type will use `Set` semantics,
-- i.e. it will disregard order and duplicate elements.
newtype ListList a = ListList {getListList :: [[a]]}
  deriving (Show, Generic)

toSet :: (Ord a) => ListList a -> Set (Set a)
toSet (ListList xss) = Set.fromList $ Set.fromList <$> xss

fromSet :: (Ord a) => Set (Set a) -> ListList a
fromSet = ListList . fmap Set.toList . Set.toList

-- | Evaluate the list-of-lists as a CNF formula.
asCNF :: (a -> Bool) -> ListList a -> Bool
asCNF f xss = all or $ toList $ f <$> xss

-- | Evaluate the list-of-lists as a DNF formula.
asDNF :: (a -> Bool) -> ListList a -> Bool
asDNF f xss = any and $ toList $ f <$> xss

-- | For our purposes we treat both levels of lists as sets,
-- and so we convert each list-of-lists to a set-of-sets before comparing.
instance (Ord a) => Eq (ListList a) where
  (==) = (==) `on` toSet

instance (Ord a) => Ord (ListList a) where
  compare = compare `on` toSet

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

-- | NOTE: This instance somewhat doesn't respect the associativity law of monads,
-- as some of the inner lists might have a different order.
-- However, due to the fact that the `Eq` instance first sorts before comparing
-- it technically still respects the law.
--
-- Why not just make it actually associative?
-- I can't figure out how.
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

instance IsList (ListList a) where
  type Item (ListList a) = [a]
  fromList = ListList
  toList (ListList xss) = xss

instance (Arbitrary a) => Arbitrary (ListList a) where
  arbitrary = fromList <$> arbitrary
  shrink = genericShrink

choices :: [[a]] -> [[a]]
choices = foldr (liftA2 (:)) [[]]
