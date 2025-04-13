{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLists #-}

module Automata.PushDown.SipserDPDASpec where

import Automata.PushDown.SipserDPDA (SipserDPDA (..))
import qualified Automata.PushDown.SipserDPDA as SDPDA
import Automata.PushDown.Util (Ended (..), end)
import Data.Alphabet
import qualified Data.Set as Set
import Data.Word (Word8)
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
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
    describe "A DPDA accepting only strings of numbers that sum to ≥ 5" $
      modifyMaxSize (`div` 10) $ do
        let dpda = dpdaSumLeastFive
        let ws' :: ([Integer] -> Bool) -> Gen [Ended Word8]
            ws' f = fmap end $ arbitrary `suchThat` (f . (fromIntegral <$>))
        prop "rejects all strings whose sum < 5" $
          let ws = ws' ((< 5) . sum)
           in (`shouldNotSatisfy` SDPDA.accepts dpda) <$> ws
        prop "accepts all strings whose sum ≥ 5" $
          let ws = ws' ((>= 5) . sum)
           in (`shouldSatisfy` SDPDA.accepts dpda) <$> ws
    describe "A DPDA popping from an empty stack" $ do
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
dpdaLoop :: SipserDPDA Word8 Word8 Word8
dpdaLoop =
  SipserDPDA
    { startState = 1,
      finalStates = Set.fromList [2],
      trans = \case
        (1, Nothing, Just n) -> Just (2, [n])
        (2, Nothing, Nothing) -> Just (3, [])
        (3, Just n, Nothing) -> Just (3, [n + 1])
        (_, _, _) -> Nothing
    }

-- A DPDA that accepts any word that sums to ≥ 5.
-- Has a designated end-of-input marker.
dpdaSumLeastFive :: SipserDPDA Word8 (Ended Word8) ()
dpdaSumLeastFive =
  SipserDPDA
    { startState = 1,
      finalStates = [7],
      trans = \case
        (1, Nothing, Just (ISymbol n)) -> Just (1, replicate (fromIntegral n) ())
        (1, Nothing, Just End) -> Just (2, [])
        (2, Just (), Nothing) -> Just (3, [])
        (3, Just (), Nothing) -> Just (4, [])
        (4, Just (), Nothing) -> Just (5, [])
        (5, Just (), Nothing) -> Just (6, [])
        (6, Just (), Nothing) -> Just (7, [])
        (7, Just (), Nothing) -> Just (7, [])
        (_, _, _) -> Nothing
    }

-- | A DPDA that pops from an empty stack.
--
-- This should accept a word iff. it has length 1.
--
-- This is to check that popping from an empty stack
-- is handled properly.
dpdaPopEmpty :: SipserDPDA Word8 () ()
dpdaPopEmpty =
  SipserDPDA
    { startState = 1,
      finalStates = Set.fromList [2],
      trans = \case
        (1, Nothing, Just ()) -> Just (2, [])
        (2, Just (), Just ()) -> Just (2, [])
        (_, _, _) -> Nothing
    }

-- | Sipser DPDA that recognizes the language {OᵏIᵏ | k ≥ 0}.
dpdakOkI :: SipserDPDA Word8 Bit Char
dpdakOkI =
  SipserDPDA
    { startState = 1,
      finalStates = [1, 4],
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
dpdaMirrored :: SipserDPDA Word8 (Either ABC ()) (ABC, Bool)
dpdaMirrored =
  SipserDPDA
    { startState = 1,
      finalStates = [4],
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
