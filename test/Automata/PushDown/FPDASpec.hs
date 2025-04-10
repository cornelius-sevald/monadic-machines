{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Automata.PushDown.FPDASpec where

import Automata.PushDown.FPDA (FPDA (..))
import qualified Automata.PushDown.FPDA as FPDA
import qualified Automata.PushDown.SipserDPDA as SDPDA
import qualified Automata.PushDown.SipserDPDASpec as SDPDASpec
import Data.Alphabet
import Data.NAry (NAry)
import Data.Word (Word8)
import Refined (Even (..), Odd (..), Refined, unrefine)
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.Util

-- | The number of states used in random PDAs.
type N = 8

-- | The type of states used in random PDAs.
type S = NAry N

-- | The input alphabet used in random PDAs.
type A = ABC

-- | The stack alphabet used in random PDAs.
type T = ABC

spec :: Spec
spec = do
  describe "Example FPDAs" $ do
    describe "An FPDA popping from an empty stack" $ do
      let fpda = fpdaPopEmpty
      it "rejects the empty string" $ do
        [] `shouldNotSatisfy` FPDA.accepts fpda
      prop "rejects all strings of a single odd number" $
        \(n :: Refined Odd Word8) ->
          [unrefine n] `shouldNotSatisfy` FPDA.accepts fpda
      prop "accepts all strings of a single even number" $
        \(n :: Refined Even Word8) ->
          [unrefine n] `shouldSatisfy` FPDA.accepts fpda
      prop "rejects all strings of length >1" $
        \(n, m, w) -> (n : m : w) `shouldNotSatisfy` FPDA.accepts fpda
    context "With L = {OᵏIᵏ | k ≥ 0}" $ do
      let (lang, langComp) = (kOkI, nonkOkI)
      let fpda = fpdakOkI
      prop "accepts strings in L" $ do
        (`shouldSatisfy` FPDA.accepts fpda) <$> lang
      prop "rejects strings not in L" $ do
        (`shouldNotSatisfy` FPDA.accepts fpda) <$> langComp
    context "With L = {w·c·w^R | c ∉ w}" $ do
      let (lang, langComp) = (mirrored, nonmirrored)
      let dpda = fpdaMirrored
      prop "accepts strings in L" $ do
        (`shouldSatisfy` FPDA.accepts dpda) <$> lang
      prop "rejects strings not in L" $ do
        (`shouldNotSatisfy` FPDA.accepts dpda) <$> langComp
  xdescribe "fromSipserDPDA" $ do
    context "With an endlessly looping Sipser DPDA" $ do
      let fpda = FPDA.fromSipserDPDA SDPDASpec.dpdaLoop
      it "rejects the empty string" $ do
        [] `shouldNotSatisfy` FPDA.accepts fpda
      prop "accepts all strings of length 1" $
        \n -> [n] `shouldSatisfy` FPDA.accepts fpda
      prop "rejects all strings of length >1" $
        \(n, m, w) -> (n : m : w) `shouldNotSatisfy` FPDA.accepts fpda
    context "With an Sipser DPDA popping from an empty stack" $ do
      let fpda = FPDA.fromSipserDPDA SDPDASpec.dpdaPopEmpty
      it "rejects the empty string" $ do
        [] `shouldNotSatisfy` FPDA.accepts fpda
      prop "accepts all strings of length 1" $
        \n -> [n] `shouldSatisfy` FPDA.accepts fpda
      prop "rejects all strings of length >1" $
        \(n, m, w) -> (n : m : w) `shouldNotSatisfy` FPDA.accepts fpda
    context "For a DPDA recognizing L = {OᵏIᵏ | k ≥ 0}" $ do
      let (lang, langComp) = (kOkI, nonkOkI)
      let fpda = FPDA.fromSipserDPDA SDPDASpec.dpdakOkI
      prop "accepts strings in L" $ do
        (`shouldSatisfy` FPDA.accepts fpda) <$> lang
      prop "rejects strings not in L" $ do
        (`shouldNotSatisfy` FPDA.accepts fpda) <$> langComp
    context "For a DPDA recognizing L = {w·c·w^R | c ∉ w}" $ do
      let (lang, langComp) = (mirrored, nonmirrored)
      let fpda = FPDA.fromSipserDPDA SDPDASpec.dpdaMirrored
      prop "accepts strings in L" $ do
        (`shouldSatisfy` FPDA.accepts fpda) <$> lang
      prop "rejects strings not in L" $ do
        (`shouldNotSatisfy` FPDA.accepts fpda) <$> langComp
    context "For a random Sipser DPDA" $ do
      prop "recognizes the same language" $ do
        \sdpda' w ->
          let sdpda = mkSipserDPDA sdpda' :: SDPDA.SipserDPDA S A T
              fpda = FPDA.fromSipserDPDA sdpda
           in FPDA.accepts fpda w `shouldBe` SDPDA.accepts sdpda w
  xdescribe "toSipserDPDA" $ do
    context "An FPDA popping from an empty stack" $ do
      let sdpda = FPDA.toSipserDPDA fpdaPopEmpty
      it "rejects the empty string" $ do
        [] `shouldNotSatisfy` SDPDA.accepts sdpda
      prop "accepts all strings of length 1" $
        \n -> [n] `shouldSatisfy` SDPDA.accepts sdpda
      prop "rejects all strings of length >1" $
        \(n, m, w) -> (n : m : w) `shouldNotSatisfy` SDPDA.accepts sdpda
    context "For a FPDA recognizing L = {OᵏIᵏ | k ≥ 0}" $ do
      let (lang, langComp) = (kOkI, nonkOkI)
      let sdpda = FPDA.toSipserDPDA fpdakOkI
      prop "accepts strings in L" $ do
        (`shouldSatisfy` SDPDA.accepts sdpda) <$> lang
      prop "rejects strings not in L" $ do
        (`shouldNotSatisfy` SDPDA.accepts sdpda) <$> langComp
    context "For a FPDA recognizing L = {w·c·w^R | c ∉ w}" $ do
      let (lang, langComp) = (mirrored, nonmirrored)
      let sdpda = FPDA.toSipserDPDA fpdaMirrored
      prop "accepts strings in L" $ do
        (`shouldSatisfy` SDPDA.accepts sdpda) <$> lang
      prop "rejects strings not in L" $ do
        (`shouldNotSatisfy` SDPDA.accepts sdpda) <$> langComp
    context "For a random FPDA" $
      modifyMaxSize (`div` 2) $ do
        prop "recognizes the same language" $ do
          \fpda' w ->
            let fpda = mkFPDA fpda' :: FPDA.FPDA S S A T
                sdpda = FPDA.toSipserDPDA fpda
             in FPDA.accepts fpda w `shouldBe` SDPDA.accepts sdpda w

{- Example FPDAs -}

-- | A FPDA that reads a single input number,
-- and then pushes that many input symbols to the stack.
-- Then proceeds to pop input symbols from the stack,
-- alternating between an accepting and non-accepting state.
--
-- Should accepts iff. the input is a single even number.
--
-- This is to check that popping from an empty stack
-- is handled properly.
fpdaPopEmpty :: FPDA Int Int Word8 ()
fpdaPopEmpty =
  FPDA
    { start = 1,
      final = [Right 1],
      transRead = \case
        (1, n) -> (Right 1, replicate (fromIntegral n) ())
        c -> error $ "invalid configuration " ++ show c,
      transPop = \case
        (1, ()) -> Right 2
        (2, ()) -> Right 1
        c -> error $ "invalid configuration " ++ show c
    }

-- | A FPDA which recognizes the language {OᵏIᵏ | k ≥ 0}.
fpdakOkI :: FPDA Int Int Bit Char
fpdakOkI =
  FPDA
    { start = 1,
      final = [Left 1, Left 4],
      transRead = \case
        (0, _) -> (Left 0, "")
        (1, O) -> (Left 2, "$")
        (1, I) -> (Left 0, "") -- FAIL: 'I' before any 'O's.
        (2, O) -> (Left 2, "+")
        (2, I) -> (Right 1, "")
        (3, O) -> (Left 0, "") -- FAIL: 'O' after 'I's.
        (3, I) -> (Right 1, "")
        (4, _) -> (Left 0, "") -- FAL: More symbols after k 'I's.
        c -> error $ "invalid configuration " ++ show c,
      transPop = \case
        (1, '+') -> (Left (3, []))
        (1, '$') -> (Left (4, []))
        c -> error $ "invalid configuration " ++ show c
    }

-- | A FPDA which recognizes the language {w·c·w^R | c ∉ w}.
fpdaMirrored :: FPDA Int (Int, ABC) (Either ABC ()) (ABC, Bool)
fpdaMirrored =
  FPDA
    { start = 1,
      final = [Left 4],
      transRead = \case
        (0, _) -> (Left 0, [])
        (1, Left a) -> (Left 2, [(a, True)])
        (1, Right ()) -> (Left 4, [])
        (2, Left a) -> (Left 2, [(a, False)])
        (2, Right ()) -> (Left 3, [])
        (3, Left a) -> (Right (1, a), [])
        (3, Right ()) -> (Left 0, [])
        (4, _) -> (Left 0, [])
        c -> error $ "invalid configuration " ++ show c,
      transPop = \case
        ((1, a), (t, b))
          | a == t && not b -> Left (3, [])
          | a == t && b -> Left (4, [])
          | otherwise -> Left (0, [])
        c -> error $ "invalid configuration " ++ show c
    }
