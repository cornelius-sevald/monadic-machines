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
        \(n, m, w) -> (n : m : w) `shouldNotSatisfy` FPDA.accepts dpda
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
-- The state 0 means the string should be rejected.
-- State 1 is the starting and an accepting state,
-- as the empty string is in the language.
-- If we read an 'O' we go to state 2 and push a dedicated
-- end-of-stack symbol '$' and a '+' to the stack.
-- In state 2 we loop as long as we read 'O's,
-- pushing the '+' symbol to the stack.
-- Once we read a 'I', we pop the '+' from the stack and
-- go to state 3, where we keep popping '+'s as long as we read 'I's.
--
-- Once we have read all input, we inspect the stack.
-- We accept only if we are in either state 1 or state 3 with an empty stack.
dpdaMirror :: FPDA Int Bit Char
dpdaMirror =
  FPDA
    { start = 1,
      final = [1],
      startSymbol = '$',
      transInput = \case
        (0, _, _) -> (0, "", True)
        (1, '$', O) -> (2, "+$", True)
        (1, '$', I) -> (0, "$", True) -- REJECT: starting with 'I'
        (2, '+', O) -> (2, "++", True)
        (2, '+', I) -> (3, "", True)
        (3, '+', I) -> (3, "", True)
        (3, _, O) -> (0, "", True) -- REJECT: 'O' after 'I's
        (3, '$', _) -> (0, "", True) -- REJECT: Too many 'I's
        c -> error $ "invalid configuration " ++ show c,
      transStack = \case
        (1, '$') -> 1
        (3, '$') -> 1
        (_, _) -> 0
    }

-- | A DPDA with a loop endlessly growing the stack.
--
-- It reads a single number, pushes it on the stack,
-- and then goes in an infinite loop pushing
-- 'n+1' on the stack, where 'n' is the previous top of the stack.
--
-- This should continue until it overflows,
-- and eventually the loop should be detected.
dpdaLoop :: FPDA Int Word8 (Maybe Word8)
dpdaLoop =
  FPDA
    { start = 1,
      final = Set.fromList [2],
      startSymbol = Nothing,
      transInput = \case
        (1, Nothing, n) -> (2, [Just n], True)
        (2, Just n, _) -> (3, [Just n], False)
        (3, Just n, _) -> (3, [Just $ n + 1], False)
        c -> error $ "invalid configuration " ++ show c,
      -- If there is anything on the stack,
      -- we move to the acceping state.
      transStack = \case
        (_, Just _) -> 2
        (s, _) -> s
    }
