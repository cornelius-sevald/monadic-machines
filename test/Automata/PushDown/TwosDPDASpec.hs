{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLists #-}

module Automata.PushDown.TwosDPDASpec where

import Automata.PushDown.TwosDPDA (TwosDPDA (..))
import qualified Automata.PushDown.TwosDPDA as TwosDPDA
import Data.Alphabet
import qualified Data.Set as Set
import Data.Word (Word8)
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Test.Util (mkLangGen)

spec :: Spec
spec = do
  describe "Example 2sDPDAs" $ do
    describe "An endlessly looping DPDA" $ do
      let dpda = dpdaLoop
      it "rejects the empty string" $ do
        [] `shouldNotSatisfy` TwosDPDA.accepts dpda
      prop "accepts all strings of length 1" $
        \n -> [n] `shouldSatisfy` TwosDPDA.accepts dpda
      prop "rejects all strings of length >1" $
        \n m w -> (n : m : w) `shouldNotSatisfy` TwosDPDA.accepts dpda
    context "With L = {OᵏIᵏ | k ≥ 0}" $ do
      let (lang, langComp) = (mirror, nonmirror)
      let dpda = dpdaMirror
      prop "accepts strings in L" $ do
        (`shouldSatisfy` TwosDPDA.accepts dpda) <$> lang
      prop "rejects strings not in L" $ do
        (`shouldNotSatisfy` TwosDPDA.accepts dpda) <$> langComp

{- Example DPDAs and associated languages -}

-- | The language {OᵏIᵏ | k ≥ 0} and its complement.
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

-- | A DPDA which recognizes the language {OᵏIᵏ | k ≥ 0}.
--
-- The state '0' means the string should be rejected.
-- The state '1' is the start state, where we read 'O's
-- and keep a count of the number via the stack.
-- The state '2' is where we read 'I's, using the
-- stack to make sure we read the correct amount
-- The state '3' is the final state, where we accept
-- the string if there is no more input.
dpdaMirror :: TwosDPDA Int Bit Char
dpdaMirror =
  TwosDPDA
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
dpdaLoop :: TwosDPDA Int Word8 Word8
dpdaLoop =
  TwosDPDA
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
