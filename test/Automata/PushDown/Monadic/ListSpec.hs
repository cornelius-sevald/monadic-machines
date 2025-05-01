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
import Data.These (These)
import Data.Void (Void, absurd)
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
      let (lang, langComp) = (langOkIk, langCompOkIk)
      let pda = pdaOkIk
      prop "accepts strings in L" $ do
        (`shouldSatisfy` ListPDA.acceptsAngelig pda) <$> lang
      prop "rejects strings not in L" $ do
        (`shouldNotSatisfy` ListPDA.acceptsAngelig pda) <$> langComp
    context "With L = {w | w is a palindrome}" $ do
      let (lang, langComp) = (langPalindromes, langCompPalindromes)
      let pda = pdaPalindromes
      prop "accepts strings in L" $ do
        (`shouldSatisfy` ListPDA.acceptsAngelig pda) <$> lang
      prop "rejects strings not in L" $ do
        (`shouldNotSatisfy` ListPDA.acceptsAngelig pda) <$> langComp
    context "With L = {w·w | w ∈ Σ*}" $
      modifyMaxSize (`div` 2) $ do
        let (lang, langComp) = (langRepeated, langCompRepeated)
        let pda = pdaNonRepeated
        prop "accepts strings not in L" $ do
          (`shouldSatisfy` ListPDA.acceptsAngelig pda) <$> langComp
        prop "rejects strings in L" $ do
          (`shouldNotSatisfy` ListPDA.acceptsAngelig pda) <$> lang
  describe "The 'invert' function" $ do
    context "With the palindrome PDA and L = {w | w is a palindrome}" $ do
      let (lang, langComp) = (langPalindromes, langCompPalindromes)
      let pda = ListPDA.invert pdaPalindromes
      prop "accepts (w. demonic non-det.) strings not in L" $ do
        (`shouldSatisfy` ListPDA.acceptsDemonic pda) <$> langComp
      prop "rejects (w. demonic non-det.) strings in L" $ do
        (`shouldNotSatisfy` ListPDA.acceptsDemonic pda) <$> lang
    context "With the non-repeated PDA and L = {w·w | w ∈ Σ*}" $
      modifyMaxSize (`div` 2) $ do
        let (lang, langComp) = (langRepeated, langCompRepeated)
        let pda = ListPDA.invert pdaNonRepeated
        prop "accepts (w. demonic non-det.) strings in L" $ do
          (`shouldSatisfy` ListPDA.acceptsDemonic pda) <$> lang
        prop "rejects (w. demonic non-det.) strings not in L" $ do
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
        let (lang, langComp) = (langOkIk, langCompOkIk)
        let pda = ListPDA.fromSipserNPDA SNPDASpec.npdaOkIk
        prop "accepts strings in L" $ do
          (`shouldSatisfy` ListPDA.acceptsAngelig pda) <$> lang
        prop "rejects strings not in L" $ do
          (`shouldNotSatisfy` ListPDA.acceptsAngelig pda) <$> langComp
      context "For a NPDA recognizing L = {w | w is a palindrome}" $ do
        let (lang, langComp) = (langPalindromes, langCompPalindromes)
        let pda = ListPDA.fromSipserNPDA SNPDASpec.npdaPalindromes
        prop "accepts strings in L" $ do
          (`shouldSatisfy` ListPDA.acceptsAngelig pda) <$> lang
        prop "rejects strings not in L" $ do
          (`shouldNotSatisfy` ListPDA.acceptsAngelig pda) <$> langComp
      context "For a random Sipser NPDA" $
        -- We have to reduce the size a crazy amount,
        -- but otherwise it runs for ages.
        -- Even at this measly size, it has caught a few bugs.
        modifyMaxSize (const 3) $ do
          prop "recognizes the same language" $ do
            \sdpda' w ->
              let snpda = mkSipserNPDA sdpda' :: SNPDA.SipserNPDA S A T
                  m = ListPDA.fromSipserNPDA snpda
               in ListPDA.acceptsAngelig m w `shouldBe` SNPDA.accepts snpda w
  describe "toSipserNPDA" $ do
    context "For a List PDA recognizing L = {OᵏIᵏ | k ≥ 0}" $ do
      let (lang, langComp) = (langOkIk, langCompOkIk)
      let snpda = ListPDA.toSipserNPDA pdaOkIk
      prop "accepts strings in L" $ do
        (`shouldSatisfy` SNPDA.accepts snpda) <$> lang
      prop "rejects strings not in L" $ do
        (`shouldNotSatisfy` SNPDA.accepts snpda) <$> langComp
    context "For a List PDA recognizing L = {w | w is a palindrome}" $ do
      let (lang, langComp) = (langPalindromes, langCompPalindromes)
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

{- Example List PDAs
 -
 - Unless otherwise specified, the PDAs are intented to be used
 - with *angelic* non-determinism.
 - -}

pdaOkIk :: ListPDA Word8 () Bit Char
pdaOkIk =
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

-- | A List PDA that recognizes the language of strings of odd length.
pdaOdd :: ListPDA (NAry 2) Void a ()
pdaOdd =
  MPDA.MonadicPDA
    { MPDA.startSymbol = _startSymbol,
      MPDA.startState = _startState,
      MPDA.finalStates = _finalStates,
      MPDA.transRead = _transRead,
      MPDA.transPop = _transPop
    }
  where
    _startSymbol = ()
    _startState = 1
    _finalStates = [2]
    _transRead = \case
      (1, _) -> [Left (2, [])]
      (2, _) -> [Left (1, [])]
      _ -> []
    _transPop (q, _) = absurd q

-- | A List PDA that recognizes the complement of the language { w·w | w ∈ { A,B,C }* },
-- when using angelig non-determinism.
--
-- Implementation taken from the description given in:
-- https://cs.stackexchange.com/a/170019.
pdaNonRepeated ::
  ListPDA
    (Maybe (Either (NAry 2) (Either (NAry 5) (NAry 5, ABC, ABC))))
    (Either Void (Either (NAry 5) (NAry 5, ABC, ABC)))
    ABC
    (These () (Bottomed ()))
pdaNonRepeated = m1 `ListPDA.union` m2
  where
    m1 = pdaOdd
    m2 =
      MPDA.MonadicPDA
        { MPDA.startSymbol = _startSymbol,
          MPDA.startState = _startState,
          MPDA.finalStates = _finalStates,
          MPDA.transRead = _transRead,
          MPDA.transPop = _transPop
        }
    _startSymbol = Bottom
    _startState = Left 1
    _finalStates = [Left 5]
    _transRead = \case
      (Left 1, a) ->
        [ Left (Left 1, [SSymbol ()]),
          Left (Right (2, a, a), [])
        ]
      (Right (2, a, _), b) ->
        [Right $ Right (2, a, b)]
      (Right (3, a, _), b) ->
        [ Left (Right (3, a, b), [SSymbol ()]),
          Right $ Right (3, a, b)
        ]
      (Left 4, _) ->
        [Right $ Left 4]
      (Left 5, _) -> []
      c -> error $ "invalid read state / input " ++ show c
    _transPop = \case
      (Right (2, a, b), SSymbol ()) ->
        [Left (Right (2, a, b), [])]
      (Right (2, a, b), Bottom) ->
        [Left (Left 5, [Bottom]) | a /= b]
          ++ [Left (Right (3, a, b), [SSymbol (), Bottom])]
      (Right (3, a, b), SSymbol ())
        | a /= b -> [Left (Left 4, [])]
        | a == b -> []
      (Left 4, SSymbol ()) ->
        [Left (Left 4, [])]
      (Left 4, Bottom) ->
        [Left (Left 5, [])]
      c -> error $ "invalid pop state / stack input " ++ show c
