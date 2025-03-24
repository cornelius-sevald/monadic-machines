{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

-- | Datatypes and functions for boolean formulae in
-- Conjunctive Normal Form (CNF) and Disjunctive Normal Form.
--
-- NOTE: We only consider *positive* boolean formulae,
-- i.e. those with no negation.
module Data.Logic.NormalForm where

import Control.Applicative
import Control.Monad (MonadPlus, join)
import qualified Data.Set as Set
import GHC.Generics (Generic)
import GHC.IsList
import Test.QuickCheck (Arbitrary)

newtype Conjunction a = Conj {getConj :: [a]}
  deriving
    ( Show,
      Eq,
      Ord,
      Semigroup,
      Monoid,
      Foldable,
      Traversable,
      Functor,
      Applicative,
      Alternative,
      Monad,
      MonadPlus,
      Arbitrary,
      Generic
    )

evalConj :: Conjunction Bool -> Bool
evalConj = and . getConj

nullConj :: Conjunction a -> Bool
nullConj (Conj []) = True
nullConj _ = False

instance IsList (Conjunction a) where
  type Item (Conjunction a) = a
  fromList = Conj
  toList = getConj

newtype Disjunction a = Disj {getDisj :: [a]}
  deriving
    ( Show,
      Eq,
      Ord,
      Semigroup,
      Monoid,
      Foldable,
      Traversable,
      Functor,
      Applicative,
      Alternative,
      Monad,
      MonadPlus,
      Arbitrary,
      Generic
    )

evalDisj :: Disjunction Bool -> Bool
evalDisj = or . getDisj

nullDisj :: Disjunction a -> Bool
nullDisj (Disj []) = True
nullDisj _ = False

instance IsList (Disjunction a) where
  type Item (Disjunction a) = a
  fromList = Disj
  toList = getDisj

{- The CNF type -}

-- | The type of positive Conjunctive Normal Form boolean formulas.
newtype CNF a = CNF {getCNF :: Conjunction (Disjunction a)}
  deriving
    ( Show,
      Semigroup,
      Monoid,
      Foldable,
      Traversable,
      Functor,
      Arbitrary,
      Generic
    )

{- CNF functions -}

evalCNF :: CNF Bool -> Bool
evalCNF (CNF p) = evalConj $ evalDisj <$> p

toDNF :: CNF a -> DNF a
toDNF = fromList . choices . toList

{- CNF instances -}

-- | We cheat by making equality convert the
-- conjunctions and disjunctions to sets before comparing.
instance (Ord a) => Eq (CNF a) where
  p == q = toSet p == toSet q
    where
      toSet = Set.fromList . (Set.fromList <$>) . toList

-- | We cheat by making comparison convert the
-- conjunctions and disjunctions to sets before comparing.
instance (Ord a) => Ord (CNF a) where
  compare p q = compare (toSet p) (toSet q)
    where
      toSet = Set.fromList . (Set.fromList <$>) . toList

instance IsList (CNF a) where
  type Item (CNF a) = [a]
  fromList = CNF . Conj . (Disj <$>)
  toList = (getDisj <$>) . getConj . getCNF

instance Applicative CNF where
  pure x = CNF (Conj [Disj [x]])
  liftA2 f p q =
    let p' = toList p
        q' = toList q
        gs = (fmap . fmap) f p'
     in fromList [g <*> y | g <- gs, y <- q']

-- | NOTE: This instance somewhat doesn't respect the associativity law of monads,
-- as some of the inner lists might have a different order.
-- However, due to the fact that the `Eq` instance first sorts before comparing
-- it technically still respects the law.
--
-- Why not just make it actually associative?
-- I can't figure out how.
instance Monad CNF where
  p >>= f =
    let ys = (fmap . fmap) (toList . f) (toList p)
        zs = concatMap (fmap concat . choices) ys
     in fromList zs

{-
-- | NOTE: This does not work.
instance Monad CNF where
  return = pure
  m >>= k =
    let cnf_cnf = k <$> m
        cnf_dnf = toDNF <$> cnf_cnf
        conj_dnf = DNF . join <$> getCNF (getDNF <$> cnf_dnf)
        conj_cnf = toCNF <$> conj_dnf
     in CNF (getCNF =<< conj_cnf)
-}

{- The DNF type -}

-- | The type of positive Conjunctive Normal Form boolean formulas.
newtype DNF a = DNF {getDNF :: Disjunction (Conjunction a)}
  deriving
    ( Show,
      Semigroup,
      Monoid,
      Foldable,
      Traversable,
      Functor,
      Arbitrary,
      Generic
    )

{- DNF functions -}

evalDNF :: DNF Bool -> Bool
evalDNF (DNF p) = evalDisj $ evalConj <$> p

toCNF :: DNF a -> CNF a
toCNF = fromList . choices . toList

{- DNF instances -}

-- | We cheat by making equality convert the
-- disjunction and conjunction to sets before comparing.
instance (Ord a) => Eq (DNF a) where
  p == q = toSet p == toSet q
    where
      toSet = Set.fromList . (Set.fromList <$>) . toList

-- | We cheat by making comparison convert the
-- disjunctions and conjunctions to sets before comparing.
instance (Ord a) => Ord (DNF a) where
  compare p q = compare (toSet p) (toSet q)
    where
      toSet = Set.fromList . (Set.fromList <$>) . toList

instance IsList (DNF a) where
  type Item (DNF a) = [a]
  fromList = DNF . Disj . (Conj <$>)
  toList = (getConj <$>) . getDisj . getDNF

instance Applicative DNF where
  pure x = DNF (Disj [Conj [x]])
  liftA2 f p q =
    let p' = toList p
        q' = toList q
        gs = (fmap . fmap) f p'
     in fromList [g <*> y | g <- gs, y <- q']

-- | NOTE: This instance somewhat doesn't respect the associativity law of monads,
-- as some of the inner lists might have a different order.
-- However, due to the fact that the `Eq` instance first sorts before comparing
-- it technically still respects the law.
--
-- Why not just make it actually associative?
-- I can't figure out how.
instance Monad DNF where
  p >>= f =
    let ys = (fmap . fmap) (toList . f) (toList p)
        zs = concatMap (fmap concat . choices) ys
     in fromList zs

{-
instance Monad DNF where
  return = pure
  m >>= k =
    let dnf_dnf = k <$> m
        dnf_cnf = toCNF <$> dnf_dnf
        conj_cnf = CNF . join <$> getDNF (getCNF <$> dnf_cnf)
        conj_dnf = toDNF <$> conj_cnf
     in DNF (getDNF =<< conj_dnf)
-}

{- Helper functions -}

choices :: [[a]] -> [[a]]
choices = foldr (liftA2 (:)) [[]]
