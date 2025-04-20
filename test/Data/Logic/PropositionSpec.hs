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
module Data.Logic.PropositionSpec where

import Data.Functor.Identity (Identity (Identity))
import qualified Data.Logic.NormalForm as NF
import Data.Logic.Proposition
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

-- | Alias for the container type
type F = Proposition

-- | Alias for the (arbitrary) contained type.
-- We just need _some_ non-trivial type with an `Arbitrary` instance.
type X = Int

-- | Like `id` but forces the type to be @F X@.
typ :: F X -> F X
typ = id

spec :: Spec
spec = do
  describe "fromCNF" $ do
    prop "preserves the truth value" $ do
      \cnf -> evaluate id (fromCNF cnf) `shouldBe` NF.evalCNF cnf
  describe "fromDNF" $ do
    prop "preserves the truth value" $ do
      \dnf -> evaluate id (fromDNF dnf) `shouldBe` NF.evalDNF dnf
  describe "toCNF" $ do
    prop "preserves the truth value" $ do
      \p -> NF.evalCNF (toCNF p) `shouldBe` evaluate id p
  describe "toDNF" $ do
    prop "preserves the truth value" $ do
      \p -> NF.evalDNF (toDNF p) `shouldBe` evaluate id p
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
    modifyMaxSize id $ do
      prop "composition" $ do
        \u' v' w ->
          let u = applyFun <$> u' :: F (X -> X)
              v = applyFun <$> v' :: F (X -> X)
           in pure (.) <*> u <*> v <*> w `shouldBe` u <*> (v <*> w)
    prop "homomorphism" $ do
      \f' x ->
        let f = applyFun f' :: X -> X
         in pure f <*> pure x `shouldBe` typ (pure (f x))
    prop "interchange" $ do
      \u' y ->
        let u = applyFun <$> u' :: F (X -> X)
         in u <*> pure y `shouldBe` pure ($ y) <*> u
  describe "Monad instance" $ do
    prop "left identity" $ do
      \k' a ->
        let k = applyFun k' :: X -> F X
         in (return a >>= k) `shouldBe` k a
    prop "right identity" $ do
      \m -> (m >>= return) `shouldBe` typ m
    modifyMaxSize id $ do
      prop "associativity" $ do
        \k' h' m ->
          let k = applyFun k' :: X -> F X
              h = applyFun h' :: X -> F X
           in (m >>= (\x -> k x >>= h)) `shouldBe` ((m >>= k) >>= h)
  -- naturality and composition were too tricky to test,
  -- so we just use hope.
  describe "Traversable instance" $ do
    prop "identity" $ do
      \x -> traverse Identity x `shouldBe` Identity (typ x)
