{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Monoid law, right identity" #-}
{-# HLINT ignore "Monoid law, left identity" #-}
{-# HLINT ignore "Functor law" #-}
{-# HLINT ignore "Use <$>" #-}
{-# HLINT ignore "Monad law, left identity" #-}
{-# HLINT ignore "Monad law, right identity" #-}
{-# HLINT ignore "Use >=>" #-}
{-# HLINT ignore "Alternative law, left identity" #-}
{-# HLINT ignore "Alternative law, right identity" #-}
module Data.Logic.NormalFormSpec where

import Data.Logic.NormalForm
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Test.Util (isqrt)

-- | Alias for the (arbitrary) contained type.
-- We just need _some_ non-trivial type with an `Arbitrary` instance.
type X = Int

spec :: Spec
spec = do
  describe "The CNF type" $ modifyMaxSize isqrt $ do
    let typ = id :: CNF X -> CNF X
    describe "toDNF" $ do
      prop "preserves the truth value" $ do
        \cnf -> evalDNF (toDNF cnf) `shouldBe` evalCNF cnf
    describe "Functor instance" $ do
      prop "identity" $ do
        \x -> fmap id x `shouldBe` typ x
      prop "composition" $ do
        \x f' g' ->
          let f = applyFun f' :: (X -> X)
              g = applyFun g' :: (X -> X)
           in fmap (f . g) x `shouldBe` (fmap f . fmap g) (typ x)
    describe "Applicative instance" $ do
      prop "identity" $ do
        \v -> pure id <*> v `shouldBe` typ v
      prop "composition" $ do
        \u' v' w ->
          let u = applyFun <$> u' :: Conjunction (X -> X)
              v = applyFun <$> v' :: Conjunction (X -> X)
           in pure (.) <*> u <*> v <*> w `shouldBe` u <*> (v <*> w)
    describe "Monad instance" $ do
      prop "left identity" $ do
        \k' a ->
          let k = applyFun k' :: X -> CNF X
           in (return a >>= k) `shouldBe` k a
      prop "right identity" $ do
        \m -> (m >>= return) `shouldBe` (m :: CNF X)
      modifyMaxSize isqrt $
        prop "associativity" $ do
          \k' h' m ->
            let k = applyFun k' :: X -> CNF X
                h = applyFun h' :: X -> CNF X
             in (m >>= (\x -> k x >>= h)) `shouldBe` ((m >>= k) >>= h)
  describe "The DNF type" $ modifyMaxSize isqrt $ do
    let typ = id :: DNF X -> DNF X
    describe "toCNF" $ do
      prop "preserves the truth value" $ do
        \dnf -> evalCNF (toCNF dnf) `shouldBe` evalDNF dnf
    describe "Functor instance" $ do
      prop "identity" $ do
        \x -> fmap id x `shouldBe` typ x
      prop "composition" $ do
        \x f' g' ->
          let f = applyFun f' :: (X -> X)
              g = applyFun g' :: (X -> X)
           in fmap (f . g) x `shouldBe` (fmap f . fmap g) (typ x)
    describe "Applicative instance" $ do
      prop "identity" $ do
        \v -> pure id <*> v `shouldBe` typ v
      prop "composition" $ do
        \u' v' w ->
          let u = applyFun <$> u' :: Conjunction (X -> X)
              v = applyFun <$> v' :: Conjunction (X -> X)
           in pure (.) <*> u <*> v <*> w `shouldBe` u <*> (v <*> w)
    describe "Monad instance" $ do
      prop "left identity" $ do
        \k' a ->
          let k = applyFun k' :: X -> DNF X
           in (return a >>= k) `shouldBe` k a
      prop "right identity" $ do
        \m -> (m >>= return) `shouldBe` (m :: DNF X)
      modifyMaxSize isqrt $
        prop "associativity" $ do
          \k' h' m ->
            let k = applyFun k' :: X -> DNF X
                h = applyFun h' :: X -> DNF X
             in (m >>= (\x -> k x >>= h)) `shouldBe` ((m >>= k) >>= h)
