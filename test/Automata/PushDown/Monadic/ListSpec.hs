{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLists #-}

module Automata.PushDown.Monadic.ListSpec where

import qualified Automata.PushDown.Monadic as MPDA
import Automata.PushDown.Monadic.List (ListPDA)
import qualified Automata.PushDown.Monadic.List as ListPDA
import qualified Automata.PushDown.SipserNPDA as SNPDA
import qualified Automata.PushDown.SipserNPDASpec as SNPDASpec
import Automata.PushDown.Util (Bottomed (..))
import Data.Alphabet
import Data.Containers.ListUtils (nubOrd)
import Data.NAry (NAry)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Universe.Class (Finite (..))
import Data.Word (Word8)
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.Util

-- | The number of states used in random PDAs.
type N = 3

-- | The type of states used in random Sipser NPDAs.
type S = NAry N

-- | The type of read states used in random List PDAs.
type R = NAry N

-- | The type of pop states used in random List PDAs.
type P = NAry N

-- | The input alphabet used in random PDAs.
type A = Bit

-- | The stack alphabet used in random PDAs.
type T = Bit

spec :: Spec
spec = do
  describe "Example List PDAs" $ do
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
    -- TODO: Move this to a dedicated `invert` test case.
    context "With L = {w | w is not a palindrome}" $ do
      let (lang, langComp) = (nonpalindromes, palindromes)
      let pda = invert pdaPalindromes
      prop "accepts strings in L" $ do
        (`shouldSatisfy` ListPDA.acceptsDemonic pda) <$> lang
      prop "rejects strings not in L" $ do
        (`shouldNotSatisfy` ListPDA.acceptsDemonic pda) <$> langComp
  describe "fromSipserNPDA" $
    modifyMaxSize (`div` 10) $ do
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
        modifyMaxSize (const 2) $ do
          prop "recognizes the same language" $ do
            \sdpda' w ->
              let snpda = mkSipserNPDA sdpda' :: SNPDA.SipserNPDA S A T
                  m = ListPDA.fromSipserNPDA snpda
               in ListPDA.acceptsAngelig m w `shouldBe` SNPDA.accepts snpda w
  describe "toSipserNPDA" $ do
    context "For a List PDA recognizing L = {OᵏIᵏ | k ≥ 0}" $ do
      let (lang, langComp) = (kOkI, nonkOkI)
      let snpda = ListPDA.toSipserNPDA pdakOkI
      prop "accepts strings in L" $ do
        (`shouldSatisfy` SNPDA.accepts snpda) <$> lang
      prop "rejects strings not in L" $ do
        (`shouldNotSatisfy` SNPDA.accepts snpda) <$> langComp
    context "For a List PDA recognizing L = {w | w is a palindrome}" $ do
      let (lang, langComp) = (palindromes, nonpalindromes)
      let snpda = ListPDA.toSipserNPDA pdaPalindromes
      prop "accepts strings in L" $ do
        (`shouldSatisfy` SNPDA.accepts snpda) <$> lang
      prop "rejects strings not in L" $ do
        (`shouldNotSatisfy` SNPDA.accepts snpda) <$> langComp
    context "For a random List PDA" $
      -- We have to reduce the size a crazy amount,
      -- but otherwise it runs for (maybe literally) ages.
      modifyMaxSize (`div` 25) $ do
        prop "recognizes the same language" $ do
          \m' w ->
            let m = mkMPDA nubOrd m' :: ListPDA.ListPDA R P A T
                sdpda = ListPDA.toSipserNPDA m
             in ListPDA.acceptsAngelig m w `shouldBe` SNPDA.accepts sdpda w

{- Example List PDAs -}

pdakOkI :: ListPDA Word8 () Bit Char
pdakOkI =
  MPDA.MonadicPDA
    { MPDA.startState = 1,
      MPDA.finalStates = [1, 4],
      MPDA.startSymbol = '$',
      MPDA.transRead = \case
        (1, O) -> [Left (2, "")]
        (1, I) -> [] -- FAIL: 'I' before any 'O's.
        (2, O) -> [Left (2, "+")]
        (2, I) -> [Right ()]
        (3, O) -> [] -- FAIL: 'O' after 'I's.
        (3, I) -> [Right ()]
        (4, _) -> [] -- FAL: More symbols after k 'I's.
        c -> error $ "invalid configuration " ++ show c,
      MPDA.transPop = \case
        ((), '$') -> [Left (4, "")]
        ((), '+') -> [Left (3, "")]
        c -> error $ "invalid configuration " ++ show c
    }

-- | A PDA that recognizes palindromes.
pdaPalindromes :: ListPDA Word8 (Either ABC ()) ABC (Bottomed ABC)
pdaPalindromes =
  MPDA.MonadicPDA
    { MPDA.startState = 1,
      MPDA.finalStates = [1, 4],
      MPDA.startSymbol = Bottom,
      MPDA.transRead = \case
        (1, a) ->
          [ Left (2, [SSymbol a]),
            Left (4, [])
          ]
        (2, a) ->
          [ Left (2, [SSymbol a]),
            Left (3, []),
            Right $ Left a
          ]
        (3, a) -> [Right $ Left a]
        (4, _) -> []
        c -> error $ "invalid configuration " ++ show c,
      MPDA.transPop = \case
        (Left _, Bottom) -> [Left (4, [])]
        (Left a, SSymbol t)
          | a == t -> [Right (Right ())]
          | otherwise -> []
        (Right (), Bottom) -> [Left (4, [])]
        (Right (), t) -> [Left (3, [t])]
    }

{- Helper functions -}
complement :: (Ord a, Finite a) => Set a -> Set a
complement xs = Set.fromList universeF `Set.difference` xs

-- Invert a List PDA, such that List PDA @m@ accepts string @w@
-- with angelic non-determinism iff. @invert m@ rejects @w@
-- with *demonic* non-determinism.
invert :: (Ord r, Finite r) => ListPDA r p a t -> ListPDA r p a t
invert m = m {MPDA.finalStates = complement $ MPDA.finalStates m}
