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
        (1, _, _) -> (2, [], Nothing)
        (2, Just n, x) -> (2, [n + 1], x)
        (2, Nothing, Nothing) -> (1, [], Nothing)
        c -> error $ "invalid configuration " ++ show c
    }
