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

getAtom :: Literal a -> a
getAtom (Variable x) = x
getAtom (Negation x) = x

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

-- | Get a set of all the atoms of a CNF.
cnfAtoms :: (Ord a) => CNF a -> Set a
cnfAtoms = Set.map getAtom . Set.unions . Set.map getDisj . getConj . getCNF

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

-- | Is this DNF satisfiable?
cnfSatisifiable :: (Ord a) => CNF a -> Bool
cnfSatisifiable cnf =
  let f vs = evalCNF $ ordFmap (`Set.member` vs) cnf
   in or $ Set.map f $ Set.powerSet $ cnfAtoms cnf

-- | Convert a CNF to an *equisatisfiable* 3-CNF.
to3CNF :: CNF Integer -> [[Literal Integer]]
to3CNF cnf =
  let cnf' = toList cnf
      cnf3 = snd $ foldl transformClause (maxVar cnf + 1, []) cnf'
   in cnf3
  where
    -- Given a fresh variable counter, transforms one clause,
    transformClause :: (Integer, [[Literal Integer]]) -> [Literal Integer] -> (Integer, [[Literal Integer]])
    transformClause (count, acc) clause
      | null clause = unsat count
      | length clause == 3 = (count, clause : acc)
      | length clause < 3 = (count, padClause clause : acc)
      | otherwise =
          let (count', clauses) = breakClause clause count
           in (count', clauses ++ acc)
    -- Pads a clause with repeated literals (does not affect satisfiability)
    padClause :: [Literal Integer] -> [Literal Integer]
    padClause lits = take 3 (lits ++ repeat (head lits))
    -- Splits a clause with more than 3 literals using new variables
    breakClause :: [Literal Integer] -> Integer -> (Integer, [[Literal Integer]])
    breakClause (l1 : l2 : l3 : rest) count =
      let newVar = count
          firstClause = [l1, l2, Variable newVar]
          (finalCounter, remainingClauses) = buildChains l3 rest (count + 1)
       in (finalCounter, firstClause : remainingClauses)
    breakClause _ count = (count, []) -- shouldn't happen
    -- Helper to chain long clauses into 3-literal clauses
    buildChains :: Literal Integer -> [Literal Integer] -> Integer -> (Integer, [[Literal Integer]])
    buildChains l [] count = (count, [[Negation (count - 1), l, l]])
    buildChains l (x : xs) count =
      let newVar = count
          clause = [Negation (count - 1), l, Variable newVar]
          (finalCounter, restClauses) = buildChains x xs (count + 1)
       in (finalCounter, clause : restClauses)
    -- A set of clauses that are unsatisfiable
    unsat count =
      let clause1 = Variable <$> [count, count, count]
          clause2 = Negation <$> [count, count, count]
       in (count + 1, [clause1, clause2])
    -- Find the highest variable ID used
    maxVar :: (Ord a, Integral a) => CNF a -> a
    maxVar = maximum . (Set.insert 1 . cnfAtoms)

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

-- | Get a set of all the atoms of a DNF.
dnfAtoms :: (Ord a) => DNF a -> Set a
dnfAtoms = Set.map getAtom . Set.unions . Set.map getConj . getDisj . getDNF

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

-- | Is this DNF satisfiable?
dnfSatisifiable :: (Ord a) => DNF a -> Bool
dnfSatisifiable dnf =
  let f vs = evalDNF $ ordFmap (`Set.member` vs) dnf
   in or $ Set.map f $ Set.powerSet $ dnfAtoms dnf

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
