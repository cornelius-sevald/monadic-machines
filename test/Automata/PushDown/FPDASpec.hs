{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLists #-}

module Automata.PushDown.FPDASpec where

import Automata.PushDown.FPDA (FPDA (..))
import qualified Automata.PushDown.FPDA as FPDA
import Data.Alphabet
import qualified Data.Set as Set
import Data.Word (Word8)
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.Util (kOkI, nonkOkI)

spec :: Spec
spec = do
  describe "Example FPDA" $ do
    describe "An endlessly looping DPDA" $ do
      let dpda = dpdaLoop
      it "rejects the empty string" $ do
        [] `shouldNotSatisfy` FPDA.accepts dpda
      prop "accepts all strings of length 1" $
        \n -> [n] `shouldSatisfy` FPDA.accepts dpda
      prop "rejects all strings of length >1" $
        \n m w -> (n : m : w) `shouldNotSatisfy` FPDA.accepts dpda
    context "With L = {OᵏIᵏ | k ≥ 0}" $ do
      let (lang, langComp) = (kOkI, nonkOkI)
      let dpda = dpdaMirror
      prop "accepts strings in L" $ do
        (`shouldSatisfy` FPDA.accepts dpda) <$> lang
      prop "rejects strings not in L" $ do
        (`shouldNotSatisfy` FPDA.accepts dpda) <$> langComp

{- Example DPDAs -}

-- | A DPDA which recognizes the language {OᵏIᵏ | k ≥ 0}.
--
-- The state '0' means the string should be rejected.
-- The state '1' is the start state, where we read 'O's
-- and keep a count of the number via the stack.
-- The state '2' is where we read 'I's, using the
-- stack to make sure we read the correct amount
-- The state '3' is the final state, where we accept
-- the string if there is no more input.
dpdaMirror :: FPDA Int Bit Char
dpdaMirror =
  FPDA
    { start = 1,
      final = Set.fromList [3],
      trans = \case
        (0, _, _) -> (0, [], True)
        (1, Nothing, Nothing) -> (3, [], True)
        (1, Nothing, Just O) -> (1, ['+'], True)
        (1, Just '+', Just O) -> (1, ['+', '+'], True)
        (1, Just '+', Just I) -> (2, [], True)
        (1, Just '+', Nothing) -> (0, [], True) -- FAIL: no input after 'O's
        (1, Nothing, Just I) -> (0, [], True) -- FAIL: read 'I' before any 'O's
        (2, Just '+', Just I) -> (2, [], True)
        (2, Nothing, Nothing) -> (3, [], True)
        (2, _, Just O) -> (0, [], True) -- FAIL: read 'O' after 'I'
        (2, Just '+', Nothing) -> (0, [], True) -- FAIL: not enough 'I's after 'O's
        (2, Nothing, Just I) -> (0, [], True) -- FAIL: too many 'I's after 'O's
        (3, Nothing, Nothing) -> (3, [], True)
        (3, _, _) -> (0, [], True) -- FAIL: either read 'O' after 'I's or too many 'I's
        c -> error $ "invalid configuration " ++ show c
    }

-- | A DPDA with a loop endlessly growing the stack.
--
-- It reads a single number, pushes it on the stack,
-- and then goes in an infinite loop pushing
-- 'n+1' on the stack, where 'n' is the previous top of the stack.
--
-- This should continue until it overflows,
-- and eventually the loop should be detected.
dpdaLoop :: FPDA Int Word8 Word8
dpdaLoop =
  FPDA
    { start = 1,
      final = Set.fromList [2],
      trans = \case
        (0, _, _) -> (0, [], True)
        (1, Nothing, Just n) -> (2, [n], True)
        (1, _, Nothing) -> (0, [], True)
        (2, Just n, Nothing) -> (3, [n], True)
        (2, _, _) -> (0, [], True)
        (3, Just n, _) -> (3, [n + 1], False)
        (3, Nothing, Nothing) -> (0, [], True)
        c -> error $ "invalid configuration " ++ show c
    }
