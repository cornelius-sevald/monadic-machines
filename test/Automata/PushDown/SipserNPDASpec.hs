{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLists #-}

module Automata.PushDown.SipserNPDASpec where

import Automata.PushDown.SipserNPDA (SipserNPDA (..))
import qualified Automata.PushDown.SipserNPDA as SNPDA
import Automata.PushDown.Util (Bottomed (..))
import Data.Alphabet
import Data.NAry
import qualified Data.Set as Set
import Data.Void (Void)
import Data.Word (Word8)
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.Util

spec :: Spec
spec = do
  describe "Example Sipser NPDAs" $ do
    describe "An endlessly looping PDA" $ do
      let npda = npdaLoop
      it "rejects the empty string" $ do
        [] `shouldNotSatisfy` SNPDA.accepts npda
      prop "accepts all strings of length 1" $
        \n -> [n] `shouldSatisfy` SNPDA.accepts npda
      prop "rejects all strings of length >1" $
        \(n, m, w) -> (n : m : w) `shouldNotSatisfy` SNPDA.accepts npda
    context "With L = {OᵏIᵏ | k ≥ 0}" $ do
      let (lang, langComp) = (langOkIk, langCompOkIk)
      let npda = npdaOkIk
      prop "accepts strings in L" $ do
        (`shouldSatisfy` SNPDA.accepts npda) <$> lang
      prop "rejects strings not in L" $ do
        (`shouldNotSatisfy` SNPDA.accepts npda) <$> langComp
    context "With L = {w | w is a palindrome}" $ do
      let (lang, langComp) = (langPalindromes, langCompPalindromes)
      let npda = npdaPalindromes
      prop "accepts strings in L" $ do
        (`shouldSatisfy` SNPDA.accepts npda) <$> lang
      prop "rejects strings not in L" $ do
        (`shouldNotSatisfy` SNPDA.accepts npda) <$> langComp
    context "With L = { w·w | w ∈ Σ* }" $
      modifyMaxSize (`div` 2) $ do
        let (lang, langComp) = (langRepeated, langCompRepeated)
        let pda = npdaNonRepeated
        prop "accepts strings not in L" $ do
          (`shouldSatisfy` SNPDA.accepts pda) <$> langComp
        prop "rejects strings in L" $ do
          (`shouldNotSatisfy` SNPDA.accepts pda) <$> lang

{- Example Sipser PDAs -}

-- | A NPDA with a loop endlessly growing the stack.
--
-- It reads a single number, pushes it on the stack,
-- and then goes in an infinite loop pushing
-- 'n+1' on the stack, where 'n' is the previous top of the stack.
--
-- This should continue until n=255,
-- where it will overflow and the loop should be detected.
npdaLoop :: SipserNPDA Word8 Word8 Word8
npdaLoop =
  SipserNPDA
    { startState = 1,
      finalStates = Set.fromList [2],
      trans = \case
        (1, Nothing, Just n) -> [(2, [n])]
        (2, Nothing, Nothing) -> [(3, [])]
        (3, Just n, Nothing) -> [(3, [n + 1])]
        (_, _, _) -> []
    }

-- | PDA from figure 2.15 of [1]
npdaOkIk :: SipserNPDA Word8 Bit Char
npdaOkIk =
  SipserNPDA
    { startState = 1,
      finalStates = [1, 4],
      trans = \case
        (1, Nothing, Nothing) -> [(2, "$")]
        (2, Nothing, Just O) -> [(2, "+")]
        (2, Just '+', Just I) -> [(3, "")]
        (3, Just '+', Just I) -> [(3, "")]
        (3, Just '$', Nothing) -> [(4, "")]
        (_, _, _) -> []
    }

-- | A PDA that recognizes palindromes.
npdaPalindromes :: SipserNPDA Word8 ABC (Either ABC ())
npdaPalindromes =
  SipserNPDA
    { startState = 1,
      finalStates = [4],
      trans = \case
        (1, Nothing, Nothing) ->
          [(2, [Right ()])]
        (2, Just (Right ()), Nothing) ->
          [(4, [])]
        (2, Nothing, Just x) ->
          [ (2, [Left x]),
            (3, [Left x]),
            (3, [])
          ]
        (3, Just (Left x), Just y)
          | x == y -> [(3, [])]
          | otherwise -> []
        (3, Just (Right ()), Nothing) ->
          [(4, [])]
        (_, _, _) -> []
    }

-- | A PDA that recognizes the language of strings of odd length.
npdaOdd :: SipserNPDA (NAry 2) a Void
npdaOdd =
  SipserNPDA
    { startState = _startState,
      finalStates = _finalStates,
      trans = _trans
    }
  where
    _startState = 1
    _finalStates = [2]
    _trans = \case
      (1, Nothing, Just _) -> [(2, [])]
      (2, Nothing, Just _) -> [(1, [])]
      _ -> []

-- | A PDA that recognizes the complement of the language { w·w | w ∈ { A,B,C }* }.
--
-- Implementation taken from the description given in:
-- https://cs.stackexchange.com/a/170019.
npdaNonRepeated ::
  SipserNPDA
    (Maybe (Either (NAry 2) (Either Bool (NAry 6, ABC, ABC))))
    ABC
    (Either Void (Bottomed ()))
npdaNonRepeated =
  -- Step 0:
  -- Non-deterministically choose to either check that w
  -- has odd length or go to step 1.
  m1 `SNPDA.union` m2
  where
    m1 :: SipserNPDA (NAry 2) a Void
    m1 = npdaOdd
    m2 :: SipserNPDA (Either Bool (NAry 6, ABC, ABC)) ABC (Bottomed ())
    m2 =
      SipserNPDA
        { startState = _startState,
          finalStates = _finalStates,
          trans = _trans
        }
    _startState = Left False
    _finalStates = [Left True]
    _trans =
      \case
        -- Step 1:
        -- Read an input symbol, and push a marker on the stack
        -- (as well as a bottom-of-stack symbol.)
        -- Go to step 2.
        (Left False, Nothing, Just a) ->
          [(Right (2, a, a), [SSymbol (), Bottom])]
        -- Step 2, case A:
        -- Push a marker on the stack for each input symbol read.
        (Right (2, _, _), Nothing, Just a) ->
          [(Right (2, a, a), [SSymbol ()])]
        -- Step 2, case B:
        -- Remember the last input read and go to step 3.
        (Right (2, a, _), Nothing, Nothing) ->
          [(Right (3, a, a), [])]
        -- Step 3, case A:
        -- Pop from the stack for each input symbol read,
        -- until the stack is empty.
        (Right (3, a, _), Just (SSymbol ()), Just b) ->
          [(Right (3, a, b), [])]
        -- Step 3, case B:
        -- If the stack is empty, go to step 4.
        (Right (3, a, b), Just Bottom, Nothing) ->
          [(Right (4, a, b), [Bottom])]
        -- Step 4, case A:
        -- Push a marker on the stack for each input symbol read.
        (Right (4, a, _), Nothing, Just b) ->
          [(Right (4, a, b), [SSymbol ()])]
        -- Step 4, case B:
        -- Remember the last input read and go to step 5.
        (Right (4, a, b), Nothing, Nothing) ->
          [(Right (5, a, b), [])]
        -- Step 5:
        -- If a = b, then reject.
        -- If a ≠ b, then go to step 6.
        (Right (5, a, b), Nothing, Nothing)
          | a == b -> []
          | a /= b -> [(Right (6, a, b), [])]
        -- Step 6, case A:
        -- Pop from the stack for each input symbol read,
        -- until the stack is empty.
        (Right (6, a, b), Just (SSymbol ()), Just _) ->
          [(Right (6, a, b), [])]
        -- Step 6, case B:
        -- If the stack is empty, accept iff. there is no input left.
        (Right (6, _, _), Just Bottom, Nothing) ->
          [(Left True, [Bottom])]
        -- This is a final state which will never read any input,
        -- i.e. it will accept only if there is no input left.
        (Left True, Nothing, Nothing) ->
          [(Left True, [])]
        _ -> []

{- Bibliography
 - ~~~~~~~~~~~~
 - [1]: M. Sipser, Introduction to the theory of computation, Third edition. Cengage Learning, 2013.
-}
