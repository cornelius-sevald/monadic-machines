{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLists #-}

module Automata.PushDown.SipserDPDASpec where

import Automata.PushDown.SipserDPDA (SipserDPDA (..))
import qualified Automata.PushDown.SipserDPDA as SDPDA
import Data.Alphabet
import qualified Data.Set as Set
import Data.Word (Word8)
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.Util (kOkI, nonkOkI)

spec :: Spec
spec = do
  describe "Example Sipser DPDAs" $ do
    describe "An endlessly looping DPDA" $ do
      let dpda = dpdaLoop
      it "rejects the empty string" $ do
        [] `shouldNotSatisfy` SDPDA.accepts dpda
      prop "accepts all strings of length 1" $
        \n -> [n] `shouldSatisfy` SDPDA.accepts dpda
      prop "rejects all strings of length >1" $
        \(n, m, w) -> (n : m : w) `shouldNotSatisfy` SDPDA.accepts dpda
    context "With L = {OᵏIᵏ | k ≥ 0}" $ do
      let (lang, langComp) = (kOkI, nonkOkI)
      let dpda = dpdaMirror
      prop "accepts strings in L" $ do
        (`shouldSatisfy` SDPDA.accepts dpda) <$> lang
      prop "rejects strings not in L" $ do
        (`shouldNotSatisfy` SDPDA.accepts dpda) <$> langComp

{- Example DPDAs and associated languages -}

dpdaMirror :: SipserDPDA Int Bit Char
dpdaMirror =
  SipserDPDA
    { start = 1,
      final = [1, 4],
      trans =
        \case
          (0, Nothing, Nothing) -> Just (0, Nothing)
          (1, Nothing, Just O) -> Just (2, Just '$')
          (1, Nothing, Just I) -> Just (0, Nothing)
          (2, Nothing, Just O) -> Just (2, Just '+')
          (2, Just '$', Just I) -> Just (4, Nothing)
          (2, Just '+', Just I) -> Just (3, Nothing)
          (3, Just '+', Just I) -> Just (3, Nothing)
          (3, Just '$', Just I) -> Just (4, Nothing)
          (3, Just _, Just O) -> Just (0, Nothing)
          (4, Nothing, Nothing) -> Just (4, Nothing)
          (_, _, _) -> Nothing
    }

-- | A DPDA with a loop endlessly growing the stack.
--
-- It reads a single number, pushes it on the stack,
-- and then goes in an infinite loop pushing
-- 'n+1' on the stack, where 'n' is the previous top of the stack.
--
-- This should continue until n=255,
-- where it will overflow and the loop should be detected.
dpdaLoop :: SipserDPDA Int Word8 Word8
dpdaLoop =
  SipserDPDA
    { start = 1,
      final = Set.fromList [2],
      trans = \case
        (1, Nothing, Just n) -> Just (2, Just n)
        (2, Nothing, Nothing) -> Just (3, Nothing)
        (3, Just n, Nothing) -> Just (3, Just (n + 1))
        (_, _, _) -> Nothing
    }
