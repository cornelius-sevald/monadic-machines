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

dpdaMirror :: TwosDPDA Int Bit Char
dpdaMirror =
  TwosDPDA
    { start = 1,
      final = Set.fromList [4],
      trans = \case
        -- State 0 is a dead state
        (0, _, _) -> (0, [], Nothing)
        -- In state 1 we can either accept immediately if
        -- there is no input, or read a 'O' and go to state 2.
        -- Readin a 'I' as the first symbol means rejection.
        (1, _, Nothing) -> (4, "", Nothing)
        (1, _, Just O) -> (2, "$", Nothing)
        (1, _, Just I) -> (0, [], Nothing)
        -- In state 2 we keep reading 'O' and pusing '+'s,
        -- until we read an 'I'. If the current stack symbol
        -- is then '$' we know that we have only read one 'O'
        -- so we go to state 4, otherwise we pop the '+' and go
        -- to state 3.
        (2, Just t, Just O) -> (2, "+" ++ [t], Nothing)
        (2, Nothing, Just O) -> (2, "+", Nothing)
        (2, Just '+', Just I) -> (3, [], Nothing)
        (2, Just '$', Just I) -> (4, [], Nothing)
        (2, _, _) -> (0, [], Nothing)
        -- In state 3 we keep reading 'I's and popping '+'s
        -- until we reach the '$' stack symbol,
        -- in which case we have read an enough 'O'
        -- so we go to state 4.
        -- Reading an 'O' or running out of input
        -- means rejection.
        (3, Just '+', Just I) -> (3, [], Nothing)
        (3, Just '$', Just I) -> (4, [], Nothing)
        (3, Just '$', Nothing) -> (4, [], Nothing)
        (3, _, Just O) -> (0, [], Nothing)
        (3, _, Nothing) -> (0, [], Nothing)
        (3, _, _) -> (0, [], Nothing)
        -- In state 4 we accept the string
        -- iff. we have read the entire input.
        (4, Nothing, Nothing) -> (4, [], Nothing)
        (4, _, _) -> (0, [], Nothing)
        c -> error $ "invalid configuration " ++ show c
    }

-- | A DPDA with a loop endlessly growing the stack.
--
-- It reads a single number, pushes it on the stack,
-- and then goes in an infinite loop pushing
-- 'n+1' on the stack, where 'n' is the previous top of the stack.
--
-- This should continue until n=255,
-- where it will overflow and the loop should be detected.
dpdaLoop :: TwosDPDA Int Word8 Word8
dpdaLoop =
  TwosDPDA
    { start = 1,
      final = Set.fromList [2],
      trans = \case
        (1, Nothing, Just n) -> (2, [n], Nothing)
        (1, _, _) -> (1, [], Nothing)
        (2, Just n, x) -> (2, [n + 1], x)
        (2, _, _) -> (2, [], Nothing)
        c -> error $ "invalid configuration " ++ show c
    }
