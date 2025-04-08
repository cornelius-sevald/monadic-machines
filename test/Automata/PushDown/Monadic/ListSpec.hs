{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLists #-}

module Automata.PushDown.Monadic.ListSpec where

import Automata.PushDown.Monadic (MonadicPDA (..))
import Automata.PushDown.Monadic.List (ListPDA)
import qualified Automata.PushDown.Monadic.List as ListPDA
import Data.Alphabet
import qualified Data.Set as Set
import Data.Word (Word8)
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.Util (kOkI, nonkOkI, nonpalindromes, palindromes)

spec :: Spec
spec = do
  describe "Example List PDAs" $ do
    describe "An endlessly looping PDA" $ do
      let pda = pdaLoop
      it "rejects the empty string" $ do
        [] `shouldNotSatisfy` ListPDA.accepts pda
      prop "accepts all strings of length 1" $
        \n -> [n] `shouldSatisfy` ListPDA.accepts pda
      prop "rejects all strings of length >1" $
        \(n, m, w) -> (n : m : w) `shouldNotSatisfy` ListPDA.accepts pda
    context "With L = {OᵏIᵏ | k ≥ 0}" $ do
      let (lang, langComp) = (kOkI, nonkOkI)
      let pda = pdakOkI
      prop "accepts strings in L" $ do
        (`shouldSatisfy` ListPDA.accepts pda) <$> lang
      prop "rejects strings not in L" $ do
        (`shouldNotSatisfy` ListPDA.accepts pda) <$> langComp
    context "With L = {w | w is a palindrome}" $ do
      let (lang, langComp) = (palindromes, nonpalindromes)
      let pda = pdaPalindromes
      prop "accepts strings in L" $ do
        (`shouldSatisfy` ListPDA.accepts pda) <$> lang
      prop "rejects strings not in L" $ do
        (`shouldNotSatisfy` ListPDA.accepts pda) <$> langComp

pdakOkI :: ListPDA Int Bit Char
pdakOkI =
  MonadicPDA
    { start = 1,
      final = Set.fromList [3],
      trans =
        \case
          (1, Nothing, Nothing) -> [(3, [], True)]
          (1, Nothing, Just O) -> [(1, ['+'], True)]
          (1, Just '+', Just O) -> [(1, ['+', '+'], True)]
          (1, Just '+', Just I) -> [(2, [], True)]
          (2, Just '+', Just I) -> [(2, [], True)]
          (2, Nothing, Nothing) -> [(3, [], True)]
          (3, Nothing, Nothing) -> [(3, [], True)]
          _ -> []
    }

-- | A PDA that recognizes palindromes.
pdaPalindromes :: ListPDA Int ABC (Either ABC ())
pdaPalindromes =
  MonadicPDA
    { start = 1,
      final = [4],
      trans = \case
        (1, Nothing, _) ->
          [(2, [Right ()], False)]
        (2, Just (Right ()), Nothing) ->
          [(4, [], False)]
        (2, Just t, Just x) ->
          [ (2, [Left x, t], True),
            (3, [Left x, t], True),
            (3, [t], True)
          ]
        (3, Just (Left x), Just y)
          | x == y -> [(3, [], True)]
          | otherwise -> []
        (3, Just (Right ()), Nothing) ->
          [(4, [], False)]
        (_, _, _) -> []
    }

-- | A PDA with a loop endlessly growing the stack.
--
-- It reads a single number, pushes it on the stack,
-- and then goes in an infinite loop pushing
-- 'n+1' on the stack, where 'n' is the previous top of the stack.
--
-- This should continue until n=255,
-- where it will overflow and the loop should be detected.
pdaLoop :: ListPDA Int Word8 Word8
pdaLoop =
  MonadicPDA
    { start = 1,
      final = Set.fromList [2],
      trans =
        \case
          (1, Nothing, Just n) -> [(2, [n], True)]
          (2, Just n, Nothing) -> [(3, [n], True)]
          (3, Just n, _) -> [(3, [n + 1], False)]
          _ -> []
    }
