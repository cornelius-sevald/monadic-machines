{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

-- | Datatypes and functions for boolean formulae in
-- Conjunctive Normal Form (CNF) and Disjunctive Normal Form (DNF).
module Data.Logic.NormalForm where

import Control.Applicative
import Data.OrdFunctor
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Universe.Class
import GHC.Generics (Generic)
import GHC.IsList
import Test.QuickCheck (Arbitrary (..), genericShrink)

data Literal a = Variable a | Negation a
  deriving (Show, Eq, Ord, Functor, Foldable, Traversable, Generic)

evalLit :: Literal Bool -> Bool
evalLit (Variable x) = x
evalLit (Negation x) = not x

instance Applicative Literal where
  pure = Variable
  liftA2 f a b =
    case (a, b) of
      (Variable x, Variable y) -> Variable (f x y)
      (Variable x, Negation y) -> Negation (f x y)
      (Negation x, Variable y) -> Negation (f x y)
      (Negation x, Negation y) -> Variable (f x y)

instance Monad Literal where
  m >>= f = case m of
    (Variable x) -> f x
    (Negation x) -> case f x of
      Variable y -> Negation y
      Negation y -> Variable y

instance (Universe a) => Universe (Literal a) where
  universe = either Variable Negation <$> universe

instance (Finite a) => Finite (Literal a)

instance (Arbitrary a) => Arbitrary (Literal a) where
  arbitrary = do
    isVar <- arbitrary
    x <- arbitrary
    pure $ if isVar then Variable x else Negation x
  shrink = genericShrink

newtype Conjunction a = Conj {getConj :: Set a}
  deriving
    ( Show,
      Eq,
      Ord,
      Semigroup,
      Monoid,
      Foldable,
      OrdFunctor,
      Arbitrary,
      Generic
    )

evalConj :: Conjunction Bool -> Bool
evalConj = and . getConj

instance (Ord a) => IsList (Conjunction a) where
  type Item (Conjunction a) = a
  fromList = Conj . Set.fromList
  toList = Set.toList . getConj

newtype Disjunction a = Disj {getDisj :: Set a}
  deriving
    ( Show,
      Eq,
      Ord,
      Semigroup,
      Monoid,
      Foldable,
      OrdFunctor,
      Arbitrary,
      Generic
    )

evalDisj :: Disjunction Bool -> Bool
evalDisj = or . getDisj

instance (Ord a) => IsList (Disjunction a) where
  type Item (Disjunction a) = a
  fromList = Disj . Set.fromList
  toList = Set.toList . getDisj

{- The CNF type -}

-- | Conjunctive Normal Form boolean formulas.
newtype CNF a = CNF {getCNF :: Conjunction (Disjunction (Literal a))}
  deriving
    ( Show,
      Eq,
      Ord,
      Semigroup,
      Monoid,
      Foldable,
      Arbitrary,
      Generic
    )

{- CNF functions -}

evalCNF :: CNF Bool -> Bool
evalCNF (CNF p) = evalConj $ ordFmap (evalDisj . ordFmap evalLit) p

toDNF :: (Ord a) => CNF a -> DNF a
toDNF = fromList . choices . toList

negateCNF :: (Ord a) => CNF a -> CNF a
negateCNF cnf =
  let dnf = DNF $ ordFmap (disjToConj . ordFmap flipLit) $ conjToDisj $ getCNF cnf
   in toCNF dnf
  where
    flipLit a = case a of Variable x -> Negation x; Negation x -> Variable x
    conjToDisj = Disj . getConj
    disjToConj = Conj . getDisj

{- CNF instances -}

instance OrdFunctor CNF where
  ordFmap f (CNF cnf) = CNF $ (ordFmap . ordFmap . fmap) f cnf

instance (Ord a) => IsList (CNF a) where
  type Item (CNF a) = [Literal a]
  fromList = CNF . fromList . (fromList <$>)
  toList = (toList <$>) . toList . getCNF

{- The DNF type -}

-- | Disjunctive Normal Form boolean formulas.
newtype DNF a = DNF {getDNF :: Disjunction (Conjunction (Literal a))}
  deriving
    ( Show,
      Eq,
      Ord,
      Semigroup,
      Monoid,
      Foldable,
      Arbitrary,
      Generic
    )

{- DNF functions -}

evalDNF :: DNF Bool -> Bool
evalDNF (DNF p) = evalDisj $ ordFmap (evalConj . ordFmap evalLit) p

toCNF :: (Ord a) => DNF a -> CNF a
toCNF = fromList . choices . toList

negateDNF :: (Ord a) => DNF a -> DNF a
negateDNF dnf =
  let cnf = CNF $ ordFmap (conjToDisj . ordFmap flipLit) $ disjToConj $ getDNF dnf
   in toDNF cnf
  where
    flipLit a = case a of Variable x -> Negation x; Negation x -> Variable x
    conjToDisj = Disj . getConj
    disjToConj = Conj . getDisj

{- DNF instances -}

instance OrdFunctor DNF where
  ordFmap f (DNF dnf) = DNF $ (ordFmap . ordFmap . fmap) f dnf

instance (Ord a) => IsList (DNF a) where
  type Item (DNF a) = [Literal a]
  fromList = DNF . fromList . (fromList <$>)
  toList = (toList <$>) . toList . getDNF

{- Helper functions -}

choices :: [[a]] -> [[a]]
choices = foldr (liftA2 (:)) [[]]
