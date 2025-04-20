{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE OverloadedLists #-}

module Data.Logic.Proposition where

import Control.Applicative (liftA2)
import Control.Monad (ap)
import Data.Logic.NormalForm (CNF (..), DNF (..))
import qualified Data.Logic.NormalForm as NF
import Data.OrdFunctor
import GHC.Generics (Generic)
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen (oneof, sized)

data Proposition v
  = Var v
  | Not (Proposition v)
  | (Proposition v) :/\: (Proposition v)
  | (Proposition v) :\/: (Proposition v)
  | Top
  | Bot
  deriving (Show, Eq, Ord, Functor, Foldable, Traversable, Generic)

evaluate :: (v -> Bool) -> Proposition v -> Bool
evaluate f (Var x) = f x
evaluate f (Not p) = not $ evaluate f p
evaluate f (p1 :/\: p2) = evaluate f p1 && evaluate f p2
evaluate f (p1 :\/: p2) = evaluate f p1 || evaluate f p2
evaluate _ Top = True
evaluate _ Bot = False

fromCNF :: (Ord a) => CNF a -> Proposition a
fromCNF (CNF cnf) =
  let conj = foldr (:/\:) Top :: NF.Conjunction (Proposition v) -> Proposition v
      disj = foldr (:\/:) Bot :: NF.Disjunction (Proposition v) -> Proposition v
      lit a = case a of NF.Variable x -> Var x; NF.Negation x -> Not (Var x)
   in conj $ ordFmap disj $ (ordFmap . ordFmap) lit cnf

fromDNF :: (Ord a) => DNF a -> Proposition a
fromDNF (DNF dnf) =
  let disj = foldr (:\/:) Bot :: NF.Disjunction (Proposition v) -> Proposition v
      conj = foldr (:/\:) Top :: NF.Conjunction (Proposition v) -> Proposition v
      lit a = case a of NF.Variable x -> Var x; NF.Negation x -> Not (Var x)
   in disj $ ordFmap conj $ (ordFmap . ordFmap) lit dnf

toCNF :: (Ord a) => Proposition a -> NF.CNF a
toCNF p = case p of
  Var x -> [[NF.Variable x]]
  Not q -> NF.negateCNF $ toCNF q
  (q1 :/\: q2) -> toCNF q1 <> toCNF q2
  (q1 :\/: q2) -> NF.toCNF $ NF.toDNF (toCNF q1) <> NF.toDNF (toCNF q2)
  Top -> []
  Bot -> [[]]

toDNF :: (Ord a) => Proposition a -> NF.DNF a
toDNF p = case p of
  Var x -> [[NF.Variable x]]
  Not q -> NF.negateDNF $ toDNF q
  (q1 :\/: q2) -> toDNF q1 <> toDNF q2
  (q1 :/\: q2) -> NF.toDNF $ NF.toCNF (toDNF q1) <> NF.toCNF (toDNF q2)
  Top -> [[]]
  Bot -> []

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
  (>>=) Bot _ = Bot

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
      leafs = [Var <$> arbitrary, pure Top, pure Bot]
  shrink = genericShrink
