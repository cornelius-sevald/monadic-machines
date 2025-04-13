{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLists #-}

module Automata.PushDown.Monadic.ListSpec where

import qualified Automata.PushDown.Monadic as MPDA
import Automata.PushDown.Monadic.List (ListPDA)
import qualified Automata.PushDown.Monadic.List as ListPDA
import qualified Automata.PushDown.SipserNPDA as SNPDA
import qualified Automata.PushDown.SipserNPDASpec as SNPDASpec
import Data.Alphabet
import Data.Containers.ListUtils (nubOrd)
import Data.NAry (NAry)
import qualified Data.Set as Set
import Data.Word (Word8)
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.Util

-- | The number of states used in random PDAs.
type N = 3

-- | The type of states used in random PDAs.
type S = NAry N

-- | The input alphabet used in random PDAs.
type A = Bit

-- | The stack alphabet used in random PDAs.
type T = Bit

spec :: Spec
spec = do
  describe "Example List PDAs" $ do
    describe "An endlessly looping List PDA" $ do
      let pda = pdaLoop
      it "rejects the empty string" $ do
        [] `shouldNotSatisfy` ListPDA.acceptsAngelig pda
      prop "accepts all strings of length 1" $
        \n -> [n] `shouldSatisfy` ListPDA.acceptsAngelig pda
      prop "rejects all strings of length >1" $
        \(n, m, w) -> (n : m : w) `shouldNotSatisfy` ListPDA.acceptsAngelig pda
    context "With L = {OᵏIᵏ | k ≥ 0}" $ do
      let (lang, langComp) = (kOkI, nonkOkI)
      let pda = pdakOkI
      prop "accepts strings in L" $ do
        (`shouldSatisfy` ListPDA.acceptsAngelig pda) <$> lang
      prop "rejects strings not in L" $ do
        (`shouldNotSatisfy` ListPDA.acceptsAngelig pda) <$> langComp
    context "With L = {w | w is a palindrome}" $ do
      let (lang, langComp) = (palindromes, nonpalindromes)
      let pda = pdaPalindromes
      prop "accepts strings in L" $ do
        (`shouldSatisfy` ListPDA.acceptsAngelig pda) <$> lang
      prop "rejects strings not in L" $ do
        (`shouldNotSatisfy` ListPDA.acceptsAngelig pda) <$> langComp
    context "With L = {w | w is not a palindrome}" $ do
      let (lang, langComp) = (nonpalindromes, palindromes)
      -- Here we flip all final states,
      -- and use demonic non-determinism.
      let pda = pdaPalindromes {MPDA.finalStates = [0, 1, 2]}
      prop "accepts strings in L" $ do
        (`shouldSatisfy` ListPDA.acceptsDemonic pda) <$> lang
      prop "rejects strings not in L" $ do
        (`shouldNotSatisfy` ListPDA.acceptsDemonic pda) <$> langComp
  describe "fromSipserNPDA" $ do
    context "With an endlessly looping Sipser NPDA" $ do
      let pda = ListPDA.fromSipserNPDA SNPDASpec.npdaLoop
      it "rejects the empty string" $ do
        [] `shouldNotSatisfy` ListPDA.acceptsAngelig pda
      prop "accepts all strings of length 1" $
        \n -> [n] `shouldSatisfy` ListPDA.acceptsAngelig pda
      prop "rejects all strings of length >1" $
        \(n, m, w) -> (n : m : w) `shouldNotSatisfy` ListPDA.acceptsAngelig pda
    context "For a NPDA recognizing L = {OᵏIᵏ | k ≥ 0}" $ do
      let (lang, langComp) = (kOkI, nonkOkI)
      let pda = ListPDA.fromSipserNPDA SNPDASpec.npdakOkI
      prop "accepts strings in L" $ do
        (`shouldSatisfy` ListPDA.acceptsAngelig pda) <$> lang
      prop "rejects strings not in L" $ do
        (`shouldNotSatisfy` ListPDA.acceptsAngelig pda) <$> langComp
    context "For a NPDA recognizing L = {w | w is a palindrome}" $ do
      let (lang, langComp) = (palindromes, nonpalindromes)
      let pda = ListPDA.fromSipserNPDA SNPDASpec.npdaPalindromes
      prop "accepts strings in L" $ do
        (`shouldSatisfy` ListPDA.acceptsAngelig pda) <$> lang
      prop "rejects strings not in L" $ do
        (`shouldNotSatisfy` ListPDA.acceptsAngelig pda) <$> langComp
    context "For a random Sipser NPDA" $
      -- We have to reduce the size a crazy amount,
      -- but otherwise it runs for ages.
      modifyMaxSize (`div` 35) $ do
        prop "recognizes the same language" $ do
          \sdpda' w ->
            let snpda = mkSipserNPDA sdpda' :: SNPDA.SipserNPDA S A T
                m = ListPDA.fromSipserNPDA snpda
             in ListPDA.acceptsAngelig m w `shouldBe` SNPDA.accepts snpda w
  describe "toSipserNPDA" $ do
    context "With an endlessly looping List PDA" $ do
      let snpda = ListPDA.toSipserNPDA pdaLoop
      it "rejects the empty string" $ do
        [] `shouldNotSatisfy` SNPDA.acceptsEOI snpda
      prop "accepts all strings of length 1" $
        \n -> [n] `shouldSatisfy` SNPDA.acceptsEOI snpda
      prop "rejects all strings of length >1" $
        \(n, m, w) -> (n : m : w) `shouldNotSatisfy` SNPDA.acceptsEOI snpda
    context "For a List PDA recognizing L = {OᵏIᵏ | k ≥ 0}" $ do
      let (lang, langComp) = (kOkI, nonkOkI)
      let snpda = ListPDA.toSipserNPDA pdakOkI
      prop "accepts strings in L" $ do
        (`shouldSatisfy` SNPDA.acceptsEOI snpda) <$> lang
      prop "rejects strings not in L" $ do
        (`shouldNotSatisfy` SNPDA.acceptsEOI snpda) <$> langComp
    context "For a List PDA recognizing L = {w | w is a palindrome}" $ do
      let (lang, langComp) = (palindromes, nonpalindromes)
      let snpda = ListPDA.toSipserNPDA pdaPalindromes
      prop "accepts strings in L" $ do
        (`shouldSatisfy` SNPDA.acceptsEOI snpda) <$> lang
      prop "rejects strings not in L" $ do
        (`shouldNotSatisfy` SNPDA.acceptsEOI snpda) <$> langComp
    context "For a random List PDA" $
      -- We have to reduce the size a crazy amount,
      -- but otherwise it runs for (maybe literally) ages.
      modifyMaxSize (`div` 25) $ do
        prop "recognizes the same language" $ do
          \m' w ->
            let m = mkMPDA nubOrd m' :: ListPDA.ListPDA S A T
                sdpda = ListPDA.toSipserNPDA m
             in ListPDA.acceptsAngelig m w `shouldBe` SNPDA.acceptsEOI sdpda w

{- Example List PDAs -}

-- | A PDA with a loop endlessly growing the stack.
--
-- It reads a single number, pushes it on the stack,
-- and then goes in an infinite loop pushing
-- 'n+1' on the stack, where 'n' is the previous top of the stack.
--
-- This should continue until n=255,
-- where it will overflow and the loop should be detected.
pdaLoop :: ListPDA Int Word8 (Maybe Word8)
pdaLoop =
  MPDA.MonadicPDA
    { MPDA.startState = 1,
      MPDA.finalStates = Set.fromList [2],
      MPDA.startSymbol = Nothing,
      MPDA.transInput = \case
        (1, Nothing, n) -> [(2, [Just n], True)]
        (2, Just n, _) -> [(3, [Just n], False)]
        (3, Just n, _) -> [(3, [Just $ n + 1], False)]
        c -> error $ "invalid configuration " ++ show c,
      -- If there is anything on the stack,
      -- we move to the acceping state.
      MPDA.transStack = \case
        (_, Just _) -> [2]
        (s, _) -> [s]
    }

pdakOkI :: ListPDA Int Bit Char
pdakOkI =
  MPDA.MonadicPDA
    { MPDA.startState = 1,
      MPDA.finalStates = Set.fromList [3],
      MPDA.startSymbol = '$',
      MPDA.transInput = \case
        (1, '$', O) -> [(1, "+$", True)]
        (1, '+', O) -> [(1, "++", True)]
        (1, '+', I) -> [(2, "", True)]
        (2, '+', I) -> [(2, "", True)]
        _ -> [],
      MPDA.transStack = \case
        (1, '$') -> [3]
        (2, '$') -> [3]
        _ -> [0]
    }

-- | A PDA that recognizes palindromes.
pdaPalindromes :: ListPDA Int ABC (Either ABC ())
pdaPalindromes =
  MPDA.MonadicPDA
    { MPDA.startState = 1,
      MPDA.finalStates = [3],
      MPDA.startSymbol = Right (),
      MPDA.transInput = \case
        (1, t, x) ->
          [ (1, [Left x, t], True),
            (2, [Left x, t], True),
            (2, [t], True)
          ]
        (2, Left x, y)
          | x == y -> [(2, [], True)]
          | otherwise -> []
        (_, _, _) -> [],
      MPDA.transStack = \case
        (1, Right ()) -> [3]
        (2, Right ()) -> [3]
        _ -> [0]
    }
