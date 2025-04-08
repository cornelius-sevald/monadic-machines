{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLists #-}

module Automata.PushDown.FPDASpec where

import Automata.PushDown.FPDA (FPDA (..))
import qualified Automata.PushDown.FPDA as FPDA
import qualified Automata.PushDown.SipserDPDASpec as SDPDASpec
import Data.Alphabet
import qualified Data.Set as Set
import Data.Word (Word8)
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.Util

spec :: Spec
spec = do
  describe "Example FPDAs" $ do
    describe "An endlessly looping FPDA" $ do
      let fpda = fpdaLoop
      it "rejects the empty string" $ do
        [] `shouldNotSatisfy` FPDA.accepts fpda
      prop "accepts all strings of length 1" $
        \n -> [n] `shouldSatisfy` FPDA.accepts fpda
      prop "rejects all strings of length >1" $
        \(n, m, w) -> (n : m : w) `shouldNotSatisfy` FPDA.accepts fpda
    describe "An FPDA popping from an empty stack" $ do
      let fpda = fpdaPopEmpty
      it "rejects the empty string" $ do
        [] `shouldNotSatisfy` FPDA.accepts fpda
      prop "accepts all strings of length 1" $
        \n -> [n] `shouldSatisfy` FPDA.accepts fpda
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
  describe "fromSipserDPDA" $ do
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
  describe "toSipserDPDA" $ do
    context "With an endlessly looping FDPDA" $ do
      let sdpda = FPDA.toSipserDPDA fpdaLoop
      it "rejects the empty string" $ do
        [] `shouldNotSatisfy` FPDA.acceptsSipserDPDA sdpda
      prop "accepts all strings of length 1" $
        \n -> [n] `shouldSatisfy` FPDA.acceptsSipserDPDA sdpda
      prop "rejects all strings of length >1" $
        \(n, m, w) -> (n : m : w) `shouldNotSatisfy` FPDA.acceptsSipserDPDA sdpda
    context "An FPDA popping from an empty stack" $ do
      let sdpda = FPDA.toSipserDPDA fpdaPopEmpty
      it "rejects the empty string" $ do
        [] `shouldNotSatisfy` FPDA.acceptsSipserDPDA sdpda
      prop "accepts all strings of length 1" $
        \n -> [n] `shouldSatisfy` FPDA.acceptsSipserDPDA sdpda
      prop "rejects all strings of length >1" $
        \(n, m, w) -> (n : m : w) `shouldNotSatisfy` FPDA.acceptsSipserDPDA sdpda
    context "For a DPDA recognizing L = {OᵏIᵏ | k ≥ 0}" $ do
      let (lang, langComp) = (kOkI, nonkOkI)
      let sdpda = FPDA.toSipserDPDA fpdakOkI
      prop "accepts strings in L" $ do
        (`shouldSatisfy` FPDA.acceptsSipserDPDA sdpda) <$> lang
      prop "rejects strings not in L" $ do
        (`shouldNotSatisfy` FPDA.acceptsSipserDPDA sdpda) <$> langComp
    context "For a DPDA recognizing L = {w·c·w^R | c ∉ w}" $ do
      let (lang, langComp) = (mirrored, nonmirrored)
      let sdpda = FPDA.toSipserDPDA fpdaMirrored
      prop "accepts strings in L" $ do
        (`shouldSatisfy` FPDA.acceptsSipserDPDA sdpda) <$> lang
      prop "rejects strings not in L" $ do
        (`shouldNotSatisfy` FPDA.acceptsSipserDPDA sdpda) <$> langComp

{- Example FPDAs -}

-- | A FPDA with a loop endlessly growing the stack.
--
-- It reads a single number, pushes it on the stack,
-- and then goes in an infinite loop pushing
-- 'n+1' on the stack, where 'n' is the previous top of the stack.
--
-- This should continue until it overflows,
-- and eventually the loop should be detected.
fpdaLoop :: FPDA Int Word8 (Maybe Word8)
fpdaLoop =
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

-- | A FPDA that pops the starting symbol,
-- and then tries to pop from the empty stack.
--
-- This should accept a word iff. it has length 1.
--
-- This is to check that popping from an empty stack
-- is handled properly.
fpdaPopEmpty :: FPDA Int () ()
fpdaPopEmpty =
  FPDA
    { start = 1,
      final = Set.fromList [1],
      startSymbol = (),
      transInput = \case
        (1, (), ()) -> (1, [], True)
        c -> error $ "invalid configuration " ++ show c,
      -- If there is anything on the stack,
      -- we move to state 0, i.e. we reject the input.
      transStack = \case
        (_, ()) -> 0
    }

-- | A FPDA which recognizes the language {OᵏIᵏ | k ≥ 0}.
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
fpdakOkI :: FPDA Int Bit Char
fpdakOkI =
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

-- | A FPDA which recognizes the language {w·c·w^R | c ∉ w}.
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
fpdaMirrored :: FPDA Int (Either ABC ()) (Either ABC ())
fpdaMirrored =
  FPDA
    { start = 1,
      final = [1],
      startSymbol = Right (),
      transInput = \case
        (0, _, _) -> (0, [], True)
        (1, t, Left a) -> (1, [Left a, t], True)
        (1, t, Right ()) -> (2, [t], True)
        (2, Left a', Left a)
          | a' == a -> (2, [], True)
          | otherwise -> (0, [], True)
        (2, _, _) -> (0, [], True)
        c -> error $ "invalid configuration " ++ show c,
      transStack = \case
        (2, Right ()) -> 1
        (_, _) -> 0
    }
