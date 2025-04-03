{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLists #-}

module Automata.PushDown.Monadic.IdentitySpec where

import Automata.PushDown.Monadic (MonadicPDA (..))
import Automata.PushDown.Monadic.Identity (IdentityPDA)
import qualified Automata.PushDown.Monadic.Identity as IdentityPDA
import Data.Alphabet
import Data.Functor.Identity
import qualified Data.Set as Set
import Data.Word (Word8)
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.Util (kOkI, nonkOkI)

spec :: Spec
spec = do
  describe "Example Identity PDA" $ do
    describe "An endlessly looping PDA" $ do
      let pda = pdaLoop
      it "rejects the empty string" $ do
        [] `shouldNotSatisfy` IdentityPDA.accepts pda
      prop "accepts all strings of length 1" $
        \n -> [n] `shouldSatisfy` IdentityPDA.accepts pda
      prop "rejects all strings of length >1" $
        \n m w -> (n : m : w) `shouldNotSatisfy` IdentityPDA.accepts pda
    context "With L = {OᵏIᵏ | k ≥ 0}" $ do
      let (lang, langComp) = (kOkI, nonkOkI)
      let pda = pdaMirror
      prop "accepts strings in L" $ do
        (`shouldSatisfy` IdentityPDA.accepts pda) <$> lang
      prop "rejects strings not in L" $ do
        (`shouldNotSatisfy` IdentityPDA.accepts pda) <$> langComp

{- Example PDAs -}

-- | A PDA which recognizes the language {OᵏIᵏ | k ≥ 0}.
--
-- The state '0' means the string should be rejected.
-- The state '1' is the start state, where we read 'O's
-- and keep a count of the number via the stack.
-- The state '2' is where we read 'I's, using the
-- stack to make sure we read the correct amount
-- The state '3' is the final state, where we accept
-- the string if there is no more input.
pdaMirror :: IdentityPDA Int Bit Char
pdaMirror =
  MonadicPDA
    { start = 1,
      final = Set.fromList [3],
      trans =
        Identity . \case
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

-- | A PDA with a loop endlessly growing the stack.
--
-- It reads a single number, pushes it on the stack,
-- and then goes in an infinite loop pushing
-- 'n+1' on the stack, where 'n' is the previous top of the stack.
--
-- This should continue until it overflows,
-- and eventually the loop should be detected.
pdaLoop :: IdentityPDA Int Word8 Word8
pdaLoop =
  MonadicPDA
    { start = 1,
      final = Set.fromList [2],
      trans =
        Identity . \case
          (0, _, _) -> (0, [], True)
          (1, Nothing, Just n) -> (2, [n], True)
          (1, _, Nothing) -> (0, [], True)
          (2, Just n, Nothing) -> (3, [n], True)
          (2, _, _) -> (0, [], True)
          (3, Just n, _) -> (3, [n + 1], False)
          (3, Nothing, Nothing) -> (0, [], True)
          c -> error $ "invalid configuration " ++ show c
    }
