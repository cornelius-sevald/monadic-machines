{-# LANGUAGE LambdaCase #-}

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
        (0, _, _) -> (0, [], Nothing)
        (1, Nothing, Nothing) -> (3, "", Nothing)
        (1, Nothing, Just O) -> (1, "+", Nothing)
        (1, Just '+', Just O) -> (1, "++", Nothing)
        (1, Just '+', Just I) -> (2, "", Nothing)
        (1, Just '+', Nothing) -> (0, "", Nothing) -- FAIL: no input after 'O's
        (1, Nothing, Just I) -> (0, "", Nothing) -- FAIL: read 'I' before any 'O's
        (2, Just '+', Just I) -> (2, "", Nothing)
        (2, Nothing, Nothing) -> (3, "", Nothing)
        (2, _, Just O) -> (0, "", Nothing) -- FAIL: read 'O' after 'I'
        (2, Just '+', Nothing) -> (0, "", Nothing) -- FAIL: not enough 'I's after 'O's
        (2, Nothing, Just I) -> (0, "", Nothing) -- FAIL: too many 'I's after 'O's
        (3, Nothing, Nothing) -> (3, "", Nothing)
        (3, _, _) -> (0, "", Nothing) -- FAIL: either read 'O' after 'I's or too many 'I's
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
        (0, _, _) -> (0, [], Nothing)
        (1, Nothing, Just n) -> (2, [n], Nothing)
        (1, _, Nothing) -> (0, [], Nothing)
        (2, Just n, Nothing) -> (3, [n], Nothing)
        (2, _, _) -> (0, [], Nothing)
        (3, Just n, x) -> (3, [n + 1], x)
        (3, Nothing, Nothing) -> (0, [], Nothing)
        c -> error $ "invalid configuration " ++ show c
    }
