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
module Data.ListListSpec where

import Data.ListList
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Test.Util (isqrt)

-- | Alias for the container type
type LL = ListList

-- | Alias for the (arbitrary) contained type.
-- We just need _some_ non-trivial type with an `Arbitrary` instance.
type X = Int

-- | Like `id` but forces the type to be @LL X@.
typ :: LL X -> LL X
typ = id

spec :: Spec
spec = do
  describe "The ListList type" $ modifyMaxSize isqrt $ do
    describe "fromList" $ do
      prop "is the opposite of toList" $ do
        \x -> (fromList . toList) x `shouldBe` typ x
    describe "toList" $ do
      prop "is the opposite of fromList" $ do
        \x -> (toList . typ . fromList) x `shouldBe` x
    describe "Semigroup instance" $ do
      prop "associativity" $ do
        \x y z -> x <> (y <> z) `shouldBe` (x <> y) <> typ z
    describe "Monoid instance" $ do
      prop "right identity" $ do
        \x -> x <> mempty `shouldBe` typ x
      prop "left identity" $ do
        \x -> mempty <> x `shouldBe` typ x
      prop "associativity" $ do
        \x y z ->
          mappend x (mappend y z)
            `shouldBe` mappend (mappend x y) (typ z)
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
          let u = applyFun <$> u' :: LL (X -> X)
              v = applyFun <$> v' :: LL (X -> X)
           in pure (.) <*> u <*> v <*> w `shouldBe` u <*> (v <*> w)
      prop "homomorphism" $ do
        \f' x ->
          let f = applyFun f' :: X -> X
           in pure f <*> pure x `shouldBe` typ (pure (f x))
      prop "interchange" $ do
        \u' y ->
          let u = applyFun <$> u' :: LL (X -> X)
           in u <*> pure y `shouldBe` pure ($ y) <*> u
    describe "Monad instance" $ do
      prop "left identity" $ do
        \k' a ->
          let k = applyFun k' :: X -> LL X
           in (return a >>= k) `shouldBe` k a
      prop "right identity" $ do
        \m -> (m >>= return) `shouldBe` (m :: LL X)
      modifyMaxSize isqrt $
        prop "associativity" $ do
          \k' h' m ->
            let k = applyFun k' :: X -> LL X
                h = applyFun h' :: X -> LL X
             in (m >>= (\x -> k x >>= h)) `shouldBe` ((m >>= k) >>= h)
