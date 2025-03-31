{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Heart where

import GHC.IsList

-- | A type that can contain either 0, 1 or 2 values of type @a@,
-- i.e. it can contain <3 values, hence the name.
--
-- The empty constructor 'Love' is named so after tennis,
-- where "love" mean zero.
data Heart a = Love | Solo a | Duo a a
  deriving (Show, Eq, Ord, Functor, Foldable)

-- | Case analysis on a 'Heart' value.
heart :: b -> (a -> b) -> (a -> a -> b) -> Heart a -> b
heart f0 f1 f2 h = case h of
  Love -> f0
  Solo x -> f1 x
  Duo x y -> f2 x y

-- | On a two-or-more element list, return @'Duo' x y@
-- where @x@ and @y@ are the first and second elements of the list resp.
-- On a singleton list, return @'Solo' x@ where @x@ is that single element.
-- On an empty list, return 'Love'.
listToHeart :: [a] -> Heart a
listToHeart xs = case xs of
  [] -> Love
  [x] -> Solo x
  (x : y : _) -> Duo x y

heartToList :: Heart a -> [a]
heartToList h = case h of
  Love -> []
  Solo x -> [x]
  Duo x y -> [x, y]

-- This instance is a little suspect as the 'fromList'
-- function is partial, but it is also pretty convenient
-- to be able to use list syntax for constructing 'Heart' values.
instance IsList (Heart a) where
  type Item (Heart a) = a
  toList = heartToList
  fromList xs =
    if length xs < 3
      then listToHeart xs
      else error "Can only convert list of length <3 to heart value."
