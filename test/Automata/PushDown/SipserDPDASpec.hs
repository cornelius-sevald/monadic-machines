{-# LANGUAGE LambdaCase #-}

module Automata.PushDown.SipserDPDASpec where

import Automata.PushDown.SipserDPDA (SipserDPDA (..))
import qualified Automata.PushDown.SipserDPDA as SDPDA
import Data.Alphabet
import qualified Data.Set as Set
import Data.Word (Word8)
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Test.Util (mkLangGen)

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
      let (lang, langComp) = (mirror, nonmirror)
      let dpda = dpdaMirror
      prop "accepts strings in L" $ do
        (`shouldSatisfy` SDPDA.accepts dpda) <$> lang
      prop "rejects strings not in L" $ do
        (`shouldNotSatisfy` SDPDA.accepts dpda) <$> langComp

{- Example DPDAs and associated languages -}

mirror, nonmirror :: Gen [Bit]
mirror = sized $ \n -> do
  k <- chooseInt (0, n `div` 2)
  pure $ replicate k O ++ replicate k I
nonmirror = mkLangGen (not . p)
  where
    p w =
      let n = length w
          k = n `div` 2
       in even n && all (== O) (take k w) && all (== I) (drop k w)

dpdaMirror :: SipserDPDA Int Bit Char
dpdaMirror =
  SipserDPDA
    { start = 1,
      -- In state 2 we have not read any input,
      -- and in state 5 we have read some 'O's
      -- followed by the same number of 'I's.
      final = Set.fromList [2, 5],
      trans = \case
        -- State 0 is a dead state.
        (0, Nothing, Nothing) -> Just (0, Nothing)
        (0, _, _) -> Nothing
        -- In state 1 we put a '$' on the stack,
        -- and move to state 2.
        (1, Nothing, Nothing) -> Just (2, Just '$')
        (1, _, _) -> Nothing
        -- In state 2, if we read a 'O' we move to state 3.
        -- If we read a 'I', we move to state 0.
        (2, Nothing, Just O) -> Just (3, Nothing)
        (2, Nothing, Just I) -> Just (0, Nothing)
        (2, _, _) -> Nothing
        -- In state 3, we keep reading 'O's
        -- and pushing '+'s to the stack,
        -- until we read a 'I', where we then either
        -- go to state 4 if there is a '+' on the stack,
        -- or directly to state 5 if there is a '$' on the stack.
        (3, Nothing, Just O) -> Just (3, Just '+')
        (3, Just '+', Just I) -> Just (4, Nothing)
        (3, Just '$', Just I) -> Just (5, Nothing)
        (3, _, _) -> Nothing
        -- In state 4, we keep reading 'I's
        -- and popping '+'s from the stack,
        -- until we reach the '$' on the bottom of the stack,
        -- where we then go to state 5.
        -- If we read a 'O', we go to state 0.
        (4, Just '+', Just I) -> Just (4, Nothing)
        (4, Just '$', Just I) -> Just (5, Nothing)
        (4, Nothing, Just O) -> Just (0, Nothing)
        (4, _, _) -> Nothing
        -- In state 5, we simply stay reading no input.
        (5, Nothing, Nothing) -> Just (5, Nothing)
        (5, _, _) -> Nothing
        c -> error $ "invalid configuration " ++ show c
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
