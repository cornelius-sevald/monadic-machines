{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}

module Data.Proposition where

import Control.Applicative (liftA2)
import Control.Monad (ap)
import GHC.Generics (Generic)
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen (oneof, sized)

data Proposition v
  = Var v
  | Not (Proposition v)
  | (Proposition v) :/\: (Proposition v)
  | (Proposition v) :\/: (Proposition v)
  | Top
  | Bottom
  deriving (Show, Eq, Ord, Functor, Foldable, Traversable, Generic)

evaluate :: (v -> Bool) -> Proposition v -> Bool
evaluate f (Var x) = f x
evaluate f (Not p) = not $ evaluate f p
evaluate f (p1 :/\: p2) = evaluate f p1 && evaluate f p2
evaluate f (p1 :\/: p2) = evaluate f p1 || evaluate f p2
evaluate _ Top = True
evaluate _ Bottom = False

instance Applicative Proposition where
  pure = Var
  (<*>) = ap

instance Monad Proposition where
  return = pure

  (>>=) (Var x) k = k x
  (>>=) (Not p) k = Not (p >>= k)
  (>>=) (p1 :/\: p2) k = (p1 >>= k) :/\: (p2 >>= k)
  (>>=) (p1 :\/: p2) k = (p1 >>= k) :\/: (p2 >>= k)
  (>>=) Top _ = Top
  (>>=) Bottom _ = Bottom

instance (Arbitrary a) => Arbitrary (Proposition a) where
  arbitrary = sized prop'
    where
      prop' 0 = oneof leafs
      prop' n
        | n > 0 =
            let subtree = prop' (n `div` 2)
                branches =
                  [ Not <$> prop' (n - 1),
                    liftA2 (:/\:) subtree subtree,
                    liftA2 (:\/:) subtree subtree
                  ]
             in oneof $ leafs ++ branches
        | otherwise = error $ "Negative size: " ++ show n
      leafs = [Var <$> arbitrary, pure Top, pure Bottom]
  shrink = genericShrink
