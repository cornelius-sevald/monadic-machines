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
import Test.Util (kOkI, mirrored, nonkOkI, nonmirrored)

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
    describe "An DPDA popping from an empty stack" $ do
      let dpda = dpdaPopEmpty
      it "rejects the empty string" $ do
        [] `shouldNotSatisfy` SDPDA.accepts dpda
      prop "accepts all strings of length 1" $
        \n -> [n] `shouldSatisfy` SDPDA.accepts dpda
      prop "rejects all strings of length >1" $
        \(n, m, w) -> (n : m : w) `shouldNotSatisfy` SDPDA.accepts dpda
    context "With L = {OᵏIᵏ | k ≥ 0}" $ do
      let (lang, langComp) = (kOkI, nonkOkI)
      let dpda = dpdakOkI
      prop "accepts strings in L" $ do
        (`shouldSatisfy` SDPDA.accepts dpda) <$> lang
      prop "rejects strings not in L" $ do
        (`shouldNotSatisfy` SDPDA.accepts dpda) <$> langComp
    context "With L = {w·c·w^R | c ∉ w}" $ do
      let (lang, langComp) = (mirrored, nonmirrored)
      let dpda = dpdaMirrored
      prop "accepts strings in L" $ do
        (`shouldSatisfy` SDPDA.accepts dpda) <$> lang
      prop "rejects strings not in L" $ do
        (`shouldNotSatisfy` SDPDA.accepts dpda) <$> langComp

{- Example DPDAs and associated languages -}

-- | A DPDA with a loop endlessly growing the stack.
-- Used to test that we can handle infinite loops.
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
        (1, Nothing, Just n) -> Just (2, [n])
        (2, Nothing, Nothing) -> Just (3, [])
        (3, Just n, Nothing) -> Just (3, [n + 1])
        (_, _, _) -> Nothing
    }

-- | A DPDA that pops from an empty stack.
--
-- This should accept a word iff. it has length 1.
--
-- This is to check that popping from an empty stack
-- is handled properly.
dpdaPopEmpty :: SipserDPDA Int () ()
dpdaPopEmpty =
  SipserDPDA
    { start = 1,
      final = Set.fromList [2],
      trans = \case
        (1, Nothing, Just ()) -> Just (2, [])
        (2, Just (), Just ()) -> Just (2, [])
        (_, _, _) -> Nothing
    }

-- | Sipser DPDA that recognizes the language {OᵏIᵏ | k ≥ 0}.
dpdakOkI :: SipserDPDA Int Bit Char
dpdakOkI =
  SipserDPDA
    { start = 1,
      final = [1, 4],
      trans =
        \case
          (0, Nothing, Nothing) -> Just (0, [])
          (1, Nothing, Just O) -> Just (2, "$")
          (1, Nothing, Just I) -> Just (0, [])
          (2, Nothing, Just O) -> Just (2, "+")
          (2, Just '$', Just I) -> Just (4, [])
          (2, Just '+', Just I) -> Just (3, [])
          (3, Just '+', Just I) -> Just (3, [])
          (3, Just '$', Just I) -> Just (4, [])
          (3, Just _, Just O) -> Just (0, [])
          (4, Nothing, Nothing) -> Just (4, [])
          (_, _, _) -> Nothing
    }

-- | Sipser DPDA that recognizes the language {w·c·w^R | c ∉ w}
-- of strings mirrored around a certain character (in this case Unit).
dpdaMirrored :: SipserDPDA Int (Either ABC ()) (ABC, Bool)
dpdaMirrored =
  SipserDPDA
    { start = 1,
      final = [4],
      trans =
        \case
          (0, Nothing, Nothing) -> Just (0, [])
          (1, Nothing, Just (Left a)) -> Just (2, [(a, True)])
          (1, Nothing, Just (Right ())) -> Just (4, [])
          (2, Nothing, Just (Left a)) -> Just (2, [(a, False)])
          (2, Nothing, Just (Right ())) -> Just (3, [])
          (3, Just (a', b), Just (Left a))
            | a == a' && b -> Just (4, [])
            | a == a' && not b -> Just (3, [])
            | otherwise -> Just (0, [])
          (3, Nothing, Just (Right ())) -> Just (0, [])
          (4, Nothing, Just _) -> Just (0, [])
          (_, _, _) -> Nothing
    }
