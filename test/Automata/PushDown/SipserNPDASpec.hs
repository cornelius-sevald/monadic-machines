{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLists #-}

module Automata.PushDown.SipserNPDASpec where

import Automata.PushDown.SipserNPDA (SipserNPDA (..))
import qualified Automata.PushDown.SipserNPDA as SNPDA
import Data.Alphabet
import qualified Data.Set as Set
import Data.Word (Word8)
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.Util (kOkI, nonkOkI, nonpalindromes, palindromes)

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
      let (lang, langComp) = (kOkI, nonkOkI)
      let npda = npdakOkI
      prop "accepts strings in L" $ do
        (`shouldSatisfy` SNPDA.accepts npda) <$> lang
      prop "rejects strings not in L" $ do
        (`shouldNotSatisfy` SNPDA.accepts npda) <$> langComp
    context "With L = {w | w is a palindrome}" $ do
      let (lang, langComp) = (palindromes, nonpalindromes)
      let npda = npdaPalindromes
      prop "accepts strings in L" $ do
        (`shouldSatisfy` SNPDA.accepts npda) <$> lang
      prop "rejects strings not in L" $ do
        (`shouldNotSatisfy` SNPDA.accepts npda) <$> langComp

{- Example Sipser PDAs -}

-- | A NPDA with a loop endlessly growing the stack.
--
-- It reads a single number, pushes it on the stack,
-- and then goes in an infinite loop pushing
-- 'n+1' on the stack, where 'n' is the previous top of the stack.
--
-- This should continue until n=255,
-- where it will overflow and the loop should be detected.
npdaLoop :: SipserNPDA Int Word8 Word8
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
npdakOkI :: SipserNPDA Int Bit Char
npdakOkI =
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
npdaPalindromes :: SipserNPDA Int ABC (Either ABC ())
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

{- Bibliography
 - ~~~~~~~~~~~~
 - [1]: M. Sipser, Introduction to the theory of computation, Third edition. Cengage Learning, 2013.
-}
