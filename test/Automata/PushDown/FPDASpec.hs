{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Automata.PushDown.FPDASpec where

import Automata.PushDown.FPDA (FPDA (..))
import qualified Automata.PushDown.FPDA as FPDA
import qualified Automata.PushDown.SipserDPDA as SDPDA
import qualified Automata.PushDown.SipserDPDASpec as SDPDASpec
import Automata.PushDown.Util
import Data.Alphabet
import qualified Data.List as List
import Data.NAry (NAry)
import Data.Word
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
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
  describe "fromSipserDPDA" $ do
    context "With an endlessly looping Sipser DPDA" $ do
      let fpda = FPDA.fromSipserDPDA SDPDASpec.dpdaLoop
      it "rejects the empty string" $ do
        [] `shouldNotSatisfy` FPDA.accepts fpda
      prop "accepts all strings of length 1" $
        \n -> [n] `shouldSatisfy` FPDA.accepts fpda
      prop "rejects all strings of length >1" $
        \(n, m, w) -> (n : m : w) `shouldNotSatisfy` FPDA.accepts fpda
    describe "With a Sipser DPDA accepting only strings of numbers that sum to ≥ 5" $
      modifyMaxSize (`div` 10) $ do
        let fpda = FPDA.fromSipserDPDA SDPDASpec.dpdaSumLeastFive
        let ws' :: ([Integer] -> Bool) -> Gen [Ended Word8]
            ws' f = fmap end $ arbitrary `suchThat` (f . (fromIntegral <$>))
        prop "rejects all strings whose sum < 5" $
          let ws = ws' ((< 5) . sum)
           in (`shouldNotSatisfy` FPDA.accepts fpda) <$> ws
        prop "accepts all strings whose sum ≥ 5" $
          let ws = ws' ((>= 5) . sum)
           in (`shouldSatisfy` FPDA.accepts fpda) <$> ws
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
      modifyMaxSize (`div` 5) $ do
        prop "recognizes the same language" $ do
          \sdpda' w ->
            let sdpda = mkSipserDPDA sdpda' :: SDPDA.SipserDPDA S A T
                fpda = FPDA.fromSipserDPDA sdpda
             in FPDA.accepts fpda w `shouldBe` SDPDA.accepts sdpda w
  describe "toSipserDPDA" $ do
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

-- | A FPDA which recognizes the language {OᵏIᵏ | k ≥ 0}.
fpdakOkI :: FPDA Word8 (Maybe Char) Bit Char
fpdakOkI =
  FPDA
    { startState = 1,
      finalStates = [1, 4],
      startSymbol = '$',
      transRead = \case
        (1, O) -> Just 'A'
        (1, I) -> Nothing -- FAIL: 'I' before any 'O's.
        (2, O) -> Just 'B'
        (2, I) -> Just 'C'
        (3, O) -> Nothing -- FAIL: 'O' after 'I's.
        (3, I) -> Just 'D'
        (4, _) -> Nothing -- FAL: More symbols after k 'I's.
        c -> error $ "invalid configuration " ++ show c,
      transPop = \case
        (Nothing, _) -> Right Nothing
        (Just 'A', '$') -> Left (2, "$")
        (Just 'B', '$') -> Left (2, "+$")
        (Just 'B', '+') -> Left (2, "++")
        (Just 'C', '$') -> Left (4, "")
        (Just 'C', '+') -> Left (3, "")
        (Just 'D', '$') -> Left (4, "")
        (Just 'D', '+') -> Left (3, "")
        c -> error $ "invalid configuration " ++ show c
    }

-- | A FPDA which recognizes the language {w·c·w^R | c ∉ w}.
fpdaMirrored :: FPDA (State ABC) (Maybe (Either ABC ())) (Either ABC ()) (Bottomed ABC)
fpdaMirrored =
  FPDA
    { startState = Start,
      finalStates = [Final],
      startSymbol = Bottom,
      transRead = \case
        (Start, a) -> Just a
        (Middle a', a)
          | Left a' == a -> Just $ Right ()
          | otherwise -> Nothing
        (Final, _) -> Nothing,
      transPop = \case
        (Nothing, _) -> Right Nothing
        (Just (Left a), t) -> Left (Start, [SSymbol a, t])
        (Just (Right ()), Bottom) -> Left (Final, [])
        (Just (Right ()), SSymbol t) -> Left (Middle t, [])
    }
