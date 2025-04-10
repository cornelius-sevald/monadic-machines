{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLists #-}

module Automata.PushDown.Monadic.IdentitySpec where

import Automata.PushDown.Monadic (MonadicPDA (..))
import Automata.PushDown.Monadic.Identity (IdentityPDA)
import qualified Automata.PushDown.Monadic.Identity as IdentityPDA
import qualified Automata.PushDown.SipserDPDA as SDPDA
import qualified Automata.PushDown.SipserDPDASpec as SDPDASpec
import Data.Alphabet
import Data.Functor.Identity
import qualified Data.Set as Set
import Data.Word (Word8)
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.Util

spec :: Spec
spec = do
  describe "Example Identity PDAs" $ do
    describe "An endlessly looping PDA" $ do
      let pda = pdaLoop
      it "rejects the empty string" $ do
        [] `shouldNotSatisfy` IdentityPDA.accepts pda
      prop "accepts all strings of length 1" $
        \n -> [n] `shouldSatisfy` IdentityPDA.accepts pda
      prop "rejects all strings of length >1" $
        \(n, m, w) -> (n : m : w) `shouldNotSatisfy` IdentityPDA.accepts pda
    describe "An PDA popping from an empty stack" $ do
      let pda = pdaPopEmpty
      it "rejects the empty string" $ do
        [] `shouldNotSatisfy` IdentityPDA.accepts pda
      prop "accepts all strings of length 1" $
        \n -> [n] `shouldSatisfy` IdentityPDA.accepts pda
      prop "rejects all strings of length >1" $
        \(n, m, w) -> (n : m : w) `shouldNotSatisfy` IdentityPDA.accepts pda
    context "With L = {OᵏIᵏ | k ≥ 0}" $ do
      let (lang, langComp) = (kOkI, nonkOkI)
      let pda = pdakOkI
      prop "accepts strings in L" $ do
        (`shouldSatisfy` IdentityPDA.accepts pda) <$> lang
      prop "rejects strings not in L" $ do
        (`shouldNotSatisfy` IdentityPDA.accepts pda) <$> langComp
    context "With L = {w·c·w^R | c ∉ w}" $ do
      let (lang, langComp) = (mirrored, nonmirrored)
      let dpda = pdaMirrored
      prop "accepts strings in L" $ do
        (`shouldSatisfy` IdentityPDA.accepts dpda) <$> lang
      prop "rejects strings not in L" $ do
        (`shouldNotSatisfy` IdentityPDA.accepts dpda) <$> langComp
  xdescribe "fromSipserDPDA" $ do
    context "With an endlessly looping Sipser DPDA" $ do
      let pda = IdentityPDA.fromSipserDPDA SDPDASpec.dpdaLoop
      it "rejects the empty string" $ do
        [] `shouldNotSatisfy` IdentityPDA.accepts pda
      prop "accepts all strings of length 1" $
        \n -> [n] `shouldSatisfy` IdentityPDA.accepts pda
      prop "rejects all strings of length >1" $
        \(n, m, w) -> (n : m : w) `shouldNotSatisfy` IdentityPDA.accepts pda
    context "With an Sipser DPDA popping from an empty stack" $ do
      let pda = IdentityPDA.fromSipserDPDA SDPDASpec.dpdaPopEmpty
      it "rejects the empty string" $ do
        [] `shouldNotSatisfy` IdentityPDA.accepts pda
      prop "accepts all strings of length 1" $
        \n -> [n] `shouldSatisfy` IdentityPDA.accepts pda
      prop "rejects all strings of length >1" $
        \(n, m, w) -> (n : m : w) `shouldNotSatisfy` IdentityPDA.accepts pda
    context "For a DPDA recognizing L = {OᵏIᵏ | k ≥ 0}" $ do
      let (lang, langComp) = (kOkI, nonkOkI)
      let pda = IdentityPDA.fromSipserDPDA SDPDASpec.dpdakOkI
      prop "accepts strings in L" $ do
        (`shouldSatisfy` IdentityPDA.accepts pda) <$> lang
      prop "rejects strings not in L" $ do
        (`shouldNotSatisfy` IdentityPDA.accepts pda) <$> langComp
    context "For a DPDA recognizing L = {w·c·w^R | c ∉ w}" $ do
      let (lang, langComp) = (mirrored, nonmirrored)
      let pda = IdentityPDA.fromSipserDPDA SDPDASpec.dpdaMirrored
      prop "accepts strings in L" $ do
        (`shouldSatisfy` IdentityPDA.accepts pda) <$> lang
      prop "rejects strings not in L" $ do
        (`shouldNotSatisfy` IdentityPDA.accepts pda) <$> langComp
  xdescribe "toSipserDPDA" $ do
    context "With an endlessly looping FDPDA" $ do
      let sdpda = IdentityPDA.toSipserDPDA pdaLoop
      it "rejects the empty string" $ do
        [] `shouldNotSatisfy` SDPDA.acceptsEOI sdpda
      prop "accepts all strings of length 1" $
        \n -> [n] `shouldSatisfy` SDPDA.acceptsEOI sdpda
      prop "rejects all strings of length >1" $
        \(n, m, w) -> (n : m : w) `shouldNotSatisfy` SDPDA.acceptsEOI sdpda
    context "An PDA popping from an empty stack" $ do
      let sdpda = IdentityPDA.toSipserDPDA pdaPopEmpty
      it "rejects the empty string" $ do
        [] `shouldNotSatisfy` SDPDA.acceptsEOI sdpda
      prop "accepts all strings of length 1" $
        \n -> [n] `shouldSatisfy` SDPDA.acceptsEOI sdpda
      prop "rejects all strings of length >1" $
        \(n, m, w) -> (n : m : w) `shouldNotSatisfy` SDPDA.acceptsEOI sdpda
    context "For a DPDA recognizing L = {OᵏIᵏ | k ≥ 0}" $ do
      let (lang, langComp) = (kOkI, nonkOkI)
      let sdpda = IdentityPDA.toSipserDPDA pdakOkI
      prop "accepts strings in L" $ do
        (`shouldSatisfy` SDPDA.acceptsEOI sdpda) <$> lang
      prop "rejects strings not in L" $ do
        (`shouldNotSatisfy` SDPDA.acceptsEOI sdpda) <$> langComp
    context "For a DPDA recognizing L = {w·c·w^R | c ∉ w}" $ do
      let (lang, langComp) = (mirrored, nonmirrored)
      let sdpda = IdentityPDA.toSipserDPDA pdaMirrored
      prop "accepts strings in L" $ do
        (`shouldSatisfy` SDPDA.acceptsEOI sdpda) <$> lang
      prop "rejects strings not in L" $ do
        (`shouldNotSatisfy` SDPDA.acceptsEOI sdpda) <$> langComp

{- Example Identity PDAs -}

-- | A PDA with a loop endlessly growing the stack.
--
-- It reads a single number, pushes it on the stack,
-- and then goes in an infinite loop pushing
-- 'n+1' on the stack, where 'n' is the previous top of the stack.
--
-- This should continue until it overflows,
-- and eventually the loop should be detected.
pdaLoop :: IdentityPDA Int Word8 (Maybe Word8)
pdaLoop =
  MonadicPDA
    { start = 1,
      final = Set.fromList [2],
      startSymbol = Nothing,
      transInput =
        Identity . \case
          (1, Nothing, n) -> (2, [Just n], True)
          (2, Just n, _) -> (3, [Just n], False)
          (3, Just n, _) -> (3, [Just $ n + 1], False)
          c -> error $ "invalid configuration " ++ show c,
      -- If there is anything on the stack,
      -- we move to the acceping state.
      transStack =
        Identity . \case
          (_, Just _) -> 2
          (s, _) -> s
    }

-- | A PDA that pops the starting symbol,
-- and then tries to pop from the empty stack.
--
-- This should accept a word iff. it has length 1.
--
-- This is to check that popping from an empty stack
-- is handled properly.
pdaPopEmpty :: IdentityPDA Int () ()
pdaPopEmpty =
  MonadicPDA
    { start = 1,
      final = Set.fromList [1],
      startSymbol = (),
      transInput =
        Identity . \case
          (1, (), ()) -> (1, [], True)
          c -> error $ "invalid configuration " ++ show c,
      -- If there is anything on the stack,
      -- we move to state 0, i.e. we reject the input.
      transStack =
        Identity . \case
          (_, ()) -> 0
    }

-- | A PDA which recognizes the language {OᵏIᵏ | k ≥ 0}.
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
pdakOkI :: IdentityPDA Int Bit Char
pdakOkI =
  MonadicPDA
    { start = 1,
      final = [1],
      startSymbol = '$',
      transInput =
        Identity . \case
          (0, _, _) -> (0, "", True)
          (1, '$', O) -> (2, "+$", True)
          (1, '$', I) -> (0, "$", True) -- REJECT: starting with 'I'
          (2, '+', O) -> (2, "++", True)
          (2, '+', I) -> (3, "", True)
          (3, '+', I) -> (3, "", True)
          (3, _, O) -> (0, "", True) -- REJECT: 'O' after 'I's
          (3, '$', _) -> (0, "", True) -- REJECT: Too many 'I's
          c -> error $ "invalid configuration " ++ show c,
      transStack =
        Identity . \case
          (1, '$') -> 1
          (3, '$') -> 1
          (_, _) -> 0
    }

-- | A PDA which recognizes the language {w·c·w^R | c ∉ w}.
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
pdaMirrored :: IdentityPDA Int (Either ABC ()) (Either ABC ())
pdaMirrored =
  MonadicPDA
    { start = 1,
      final = [1],
      startSymbol = Right (),
      transInput =
        Identity . \case
          (0, _, _) -> (0, [], True)
          (1, t, Left a) -> (1, [Left a, t], True)
          (1, t, Right ()) -> (2, [t], True)
          (2, Left a', Left a)
            | a' == a -> (2, [], True)
            | otherwise -> (0, [], True)
          (2, _, _) -> (0, [], True)
          c -> error $ "invalid configuration " ++ show c,
      transStack =
        Identity . \case
          (2, Right ()) -> 1
          (_, _) -> 0
    }
