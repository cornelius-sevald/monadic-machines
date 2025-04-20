{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Functor law" #-}
module Data.Logic.NormalFormSpec where

import Data.Logic.NormalForm
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.Util (isqrt)

-- | Alias for the (arbitrary) contained type.
-- We just need _some_ non-trivial type with an `Arbitrary` instance.
type X = Int

spec :: Spec
spec = do
  describe "The CNF type" $ modifyMaxSize isqrt $ do
    describe "toDNF" $ do
      prop "preserves the truth value" $ do
        \cnf -> evalDNF (toDNF cnf) `shouldBe` evalCNF cnf
    describe "negateCNF" $ do
      prop "negates the truth value of the CNF" $ do
        \cnf -> evalCNF (negateCNF cnf) `shouldBe` not (evalCNF cnf)
  describe "The DNF type" $ modifyMaxSize isqrt $ do
    describe "toCNF" $ do
      prop "preserves the truth value" $ do
        \dnf -> evalCNF (toCNF dnf) `shouldBe` evalDNF dnf
    describe "negateDNF" $ do
      prop "negates the truth value of the DNF" $ do
        \dnf -> evalDNF (negateDNF dnf) `shouldBe` not (evalDNF dnf)
