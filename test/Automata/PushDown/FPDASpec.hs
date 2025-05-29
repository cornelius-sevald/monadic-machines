{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Automata.PushDown.FPDASpec where

import qualified Automata.PushDown.DPDA as DPDA
import qualified Automata.PushDown.DPDASpec as DPDASpec
import Automata.PushDown.FPDA (FPDA (..))
import qualified Automata.PushDown.FPDA as FPDA
import Automata.PushDown.Util
import Data.Alphabet
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
      let (lang, langComp) = (langOkIk, langCompOkIk)
      let fpda = fpdaOkIk
      prop "accepts strings in L" $ do
        (`shouldSatisfy` FPDA.accepts fpda) <$> lang
      prop "rejects strings not in L" $ do
        (`shouldNotSatisfy` FPDA.accepts fpda) <$> langComp
    context "With L = {w·c·w^R | c ∉ w}" $ do
      let (lang, langComp) = (langMirrored, langCompMirrored)
      let dpda = fpdaMirrored
      prop "accepts strings in L" $ do
        (`shouldSatisfy` FPDA.accepts dpda) <$> lang
      prop "rejects strings not in L" $ do
        (`shouldNotSatisfy` FPDA.accepts dpda) <$> langComp
  describe "fromDPDA" $ do
    context "With an endlessly looping DPDA" $ do
      let fpda = FPDA.fromDPDA DPDASpec.dpdaLoop
      it "rejects the empty string" $ do
        [] `shouldNotSatisfy` FPDA.accepts fpda
      prop "accepts all strings of length 1" $
        \n -> [n] `shouldSatisfy` FPDA.accepts fpda
      prop "rejects all strings of length >1" $
        \(n, m, w) -> (n : m : w) `shouldNotSatisfy` FPDA.accepts fpda
    describe "With a DPDA accepting only strings of numbers that sum to ≥ 5" $
      modifyMaxSize (`div` 10) $ do
        let fpda = FPDA.fromDPDA DPDASpec.dpdaSumLeastFive
        let ws' :: ([Integer] -> Bool) -> Gen [Ended Word8]
            ws' f = fmap end $ arbitrary `suchThat` (f . (fromIntegral <$>))
        prop "rejects all strings whose sum < 5" $
          let ws = ws' ((< 5) . sum)
           in (`shouldNotSatisfy` FPDA.accepts fpda) <$> ws
        prop "accepts all strings whose sum ≥ 5" $
          let ws = ws' ((>= 5) . sum)
           in (`shouldSatisfy` FPDA.accepts fpda) <$> ws
    context "With a DPDA popping from an empty stack" $ do
      let fpda = FPDA.fromDPDA DPDASpec.dpdaPopEmpty
      it "rejects the empty string" $ do
        [] `shouldNotSatisfy` FPDA.accepts fpda
      prop "accepts all strings of length 1" $
        \n -> [n] `shouldSatisfy` FPDA.accepts fpda
      prop "rejects all strings of length >1" $
        \(n, m, w) -> (n : m : w) `shouldNotSatisfy` FPDA.accepts fpda
    context "For a DPDA recognizing L = {OᵏIᵏ | k ≥ 0}" $ do
      let (lang, langComp) = (langOkIk, langCompOkIk)
      let fpda = FPDA.fromDPDA DPDASpec.dpdaOkIk
      prop "accepts strings in L" $ do
        (`shouldSatisfy` FPDA.accepts fpda) <$> lang
      prop "rejects strings not in L" $ do
        (`shouldNotSatisfy` FPDA.accepts fpda) <$> langComp
    context "For a DPDA recognizing L = {w·c·w^R | c ∉ w}" $ do
      let (lang, langComp) = (langMirrored, langCompMirrored)
      let fpda = FPDA.fromDPDA DPDASpec.dpdaMirrored
      prop "accepts strings in L" $ do
        (`shouldSatisfy` FPDA.accepts fpda) <$> lang
      prop "rejects strings not in L" $ do
        (`shouldNotSatisfy` FPDA.accepts fpda) <$> langComp
    context "For a random DPDA" $ do
      modifyMaxSize (`div` 5) $ do
        prop "recognizes the same language" $ do
          \sdpda' w ->
            let sdpda = mkDPDA sdpda' :: DPDA.DPDA S A T
                fpda = FPDA.fromDPDA sdpda
             in FPDA.accepts fpda w `shouldBe` DPDA.accepts sdpda w
  describe "toDPDA" $ do
    context "For a FPDA recognizing L = {OᵏIᵏ | k ≥ 0}" $ do
      let (lang, langComp) = (langOkIk, langCompOkIk)
      let sdpda = FPDA.toDPDA fpdaOkIk
      prop "accepts strings in L" $ do
        (`shouldSatisfy` DPDA.accepts sdpda) <$> lang
      prop "rejects strings not in L" $ do
        (`shouldNotSatisfy` DPDA.accepts sdpda) <$> langComp
    context "For a FPDA recognizing L = {w·c·w^R | c ∉ w}" $ do
      let (lang, langComp) = (langMirrored, langCompMirrored)
      let sdpda = FPDA.toDPDA fpdaMirrored
      prop "accepts strings in L" $ do
        (`shouldSatisfy` DPDA.accepts sdpda) <$> lang
      prop "rejects strings not in L" $ do
        (`shouldNotSatisfy` DPDA.accepts sdpda) <$> langComp
    context "For a random FPDA" $
      modifyMaxSize (`div` 2) $ do
        prop "recognizes the same language" $ do
          \fpda' w ->
            let fpda = mkFPDA fpda' :: FPDA.FPDA S S A T
                sdpda = FPDA.toDPDA fpda
             in FPDA.accepts fpda w `shouldBe` DPDA.accepts sdpda w

{- Example FPDAs -}

-- | A FPDA which recognizes the language {OᵏIᵏ | k ≥ 0}.
--
-- The idea is, after reading the initial 'O' (if there is one),
-- we push a marker symbol ('+') on the stack for each subsequent
-- 'O' read. When reading the first 'I', we reject any further 'O's,
-- and for each 'I' we read, we inspect the stack.
-- If it is a '+', we pop it from the stack. If it is a '$',
-- then we have read an equal number of 'O's followed by 'I's,
-- and so we accept, iff. there is no more input.
--
-- The read states are as follows:
--   State 1, the initial state.
--     Either reads a single 'O' and progresses
--     to state 2, rejects when reading a 'I',
--     or accepts on no input.
--   State 2.
--     Keeps reading 'O's, pushing a '+' to the
--     stack for each 'O' read.
--     If an 'I' is read, goes to the pop state.
--   State 3.
--     Keeps reading 'I's, popping a '+' from the
--     stack for each 'I' read (via the pop state).
--     Rejects when reading a 'O'.
--   State 4, a final state that rejects on
--     any further input.
--
-- There is only a single pop state,
-- which goes goes to state 4 (a final state)
-- if the bottom of the stack is reached
-- (signaled with the '$' symbol).
-- Otherwise (on a '+' symbol), it goes to state 3.
fpdaOkIk :: FPDA (Maybe Int) () Bit Char
fpdaOkIk =
  FPDA
    { startState = Just 1,
      finalStates = [Just 1, Just 4],
      startSymbol = '$',
      transRead = \case
        (Nothing, _) -> Left (Nothing, "")
        (Just 1, O) -> Left (Just 2, "")
        (Just 1, I) -> Left (Nothing, "") -- FAIL: 'I' before any 'O's.
        (Just 2, O) -> Left (Just 2, "+")
        (Just 2, I) -> Right ()
        (Just 3, O) -> Left (Nothing, []) -- FAIL: 'O' after 'I's.
        (Just 3, I) -> Right ()
        (Just 4, _) -> Left (Nothing, []) -- FAL: More symbols after k 'I's.
        c -> error $ "invalid configuration " ++ show c,
      transPop = \case
        ((), '$') -> Left (Just 4, "")
        ((), '+') -> Left (Just 3, "")
        c -> error $ "invalid configuration " ++ show c
    }

-- | A FPDA which recognizes the language {w·c·w^R | c ∉ w}.
--
-- The idea is that we put each input symbol read onto
-- the stack, until we hit the 'c' symbol.
-- From there, we pop symbols from the stack,
-- and move to a state indexed by that stack symbol.
-- From this state, we can compare the stack symbol
-- with the next symbol to read.
--
-- The read states are as follows:
--   State (Left False), the initial state.
--     Reads input symbols, pushing them to the stack,
--     until it reads the 'c' symbol.
--   State (Left True), the final state.
--     Moves to the rejecting state if any further input is read.
--   State (Right t), where 't' is a stack symbol.
--     Checks that the associated stack symbol
--     matches the input symbol, moving to the rejecting state if not.
--
-- The pop states are as follows:
--   State 0, the rejecting state.
--   State 1.
--     When reading stack symbol 't',
--     moves to read state 'Right t'.
--     When reaching the bottom of the stack,
--     moves to the final state, 'Left True'.
fpdaMirrored :: FPDA (Either Bool ABC) Int (Either ABC ()) (Bottomed ABC)
fpdaMirrored =
  FPDA
    { startState = Left False,
      finalStates = [Left True],
      startSymbol = Bottom,
      transRead = \case
        (Left False, Left a) -> Left (Left False, [SSymbol a])
        (Left False, Right ()) -> Right 1
        (Right t, a)
          | Left t == a -> Right 1
          | otherwise -> Right 0
        (Left True, _) -> Right 0,
      transPop = \case
        (0, _) -> Right 0
        (1, Bottom) -> Left (Left True, [])
        (1, SSymbol t) -> Left (Right t, [])
        c -> error $ "invalid configuration " ++ show c
    }
