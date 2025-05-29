{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLists #-}

module Automata.PushDown.Monadic.ListSpec where

import qualified Automata.PushDown.Monadic as MPDA
import Automata.PushDown.Monadic.List (ListPDA)
import qualified Automata.PushDown.Monadic.List as ListPDA
import qualified Automata.PushDown.NPDA as NPDA
import qualified Automata.PushDown.NPDASpec as NPDASpec
import Automata.PushDown.Util (Bottomed (..))
import Data.Alphabet
import Data.Containers.ListUtils (nubOrd)
import Data.Maybe (maybeToList)
import Data.NAry (NAry)
import qualified Data.NAry as NAry
import qualified Data.Set as Set
import Data.These (These)
import Data.Void (Void, absurd)
import Data.Word (Word8)
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.Util

-- | The number of states used in random PDAs.
type N = 3

-- | The type of states used in random NPDAs.
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
      prop "accepts(∃) strings in L" $ do
        (`shouldSatisfy` ListPDA.acceptsAngelig pda) <$> lang
      prop "rejects(∃) strings not in L" $ do
        (`shouldNotSatisfy` ListPDA.acceptsAngelig pda) <$> langComp
    context "With L = {w | w is a palindrome}" $ do
      let (lang, langComp) = (langPalindromes, langCompPalindromes)
      let pda = pdaPalindromes
      prop "accepts(∃) strings in L" $ do
        (`shouldSatisfy` ListPDA.acceptsAngelig pda) <$> lang
      prop "rejects(∃) strings not in L" $ do
        (`shouldNotSatisfy` ListPDA.acceptsAngelig pda) <$> langComp
    context "With L = {w·w | w ∈ Σ*}" $
      modifyMaxSize (`div` 2) $ do
        let (lang, langComp) = (langRepeated, langCompRepeated)
        let pda = pdaNonRepeated
        prop "accepts(∃) strings not in L" $ do
          (`shouldSatisfy` ListPDA.acceptsAngelig pda) <$> langComp
        prop "rejects(∃) strings in L" $ do
          (`shouldNotSatisfy` ListPDA.acceptsAngelig pda) <$> lang
    context "With L = { AⁿBⁿCⁿ | n ≥ 0 }" $ do
      let (lang, langComp) = (langEQ, langCompEQ)
      let pda = pdaEQ
      prop "accepts(∀) strings in L" $ do
        (`shouldSatisfy` ListPDA.acceptsDemonic pda) <$> lang
      prop "rejects(∀) strings not in L" $ do
        (`shouldNotSatisfy` ListPDA.acceptsDemonic pda) <$> langComp
  describe "The 'invert' function" $ do
    context "With the palindrome PDA and L = {w | w is a palindrome}" $ do
      let (lang, langComp) = (langPalindromes, langCompPalindromes)
      let pda = ListPDA.invert pdaPalindromes
      prop "accepts(∀) strings not in L" $ do
        (`shouldSatisfy` ListPDA.acceptsDemonic pda) <$> langComp
      prop "rejects(∀) strings in L" $ do
        (`shouldNotSatisfy` ListPDA.acceptsDemonic pda) <$> lang
    context "With the non-repeated PDA and L = {w·w | w ∈ Σ*}" $
      modifyMaxSize (`div` 2) $ do
        let (lang, langComp) = (langRepeated, langCompRepeated)
        let pda = ListPDA.invert pdaNonRepeated
        prop "accepts(∀) strings in L" $ do
          (`shouldSatisfy` ListPDA.acceptsDemonic pda) <$> lang
        prop "rejects(∀) strings not in L" $ do
          (`shouldNotSatisfy` ListPDA.acceptsDemonic pda) <$> langComp
    context "With the EQ PDA and L = { AⁿBⁿCⁿ | n ≥ 0 }" $ do
      let (lang, langComp) = (langEQ, langCompEQ)
      let pda = ListPDA.invert pdaEQ
      prop "accepts(∃) strings not in L" $ do
        (`shouldSatisfy` ListPDA.acceptsAngelig pda) <$> langComp
      prop "rejects(∃) strings in L" $ do
        (`shouldNotSatisfy` ListPDA.acceptsAngelig pda) <$> lang
    context "For a random List PDA M" $
      -- We have to reduce the size a crazy amount,
      -- but otherwise it runs for (maybe literally) ages.
      modifyMaxSize (`div` 20) $ do
        prop "M accepts(∃) iff. invert M rejects(∀)" $ do
          \m' w ->
            let m = mkMPDA nubOrd m' :: ListPDA.ListPDA R P A T
                mInv = ListPDA.invert m
             in ListPDA.acceptsAngelig m w `shouldNotBe` ListPDA.acceptsDemonic mInv w
        prop "M accepts(∀) iff. invert M rejects(∃)" $ do
          \m' w ->
            let m = mkMPDA nubOrd m' :: ListPDA.ListPDA R P A T
                mInv = ListPDA.invert m
             in ListPDA.acceptsDemonic m w `shouldNotBe` ListPDA.acceptsAngelig mInv w
  describe "fromNPDA" $
    modifyMaxSize (`div` 10) $ do
      context "With an looping PDA that eventually overflows the stack symbol" $ do
        let pda = ListPDA.fromNPDA NPDASpec.npdaOverflow
        it "rejects(∃) the empty string" $ do
          [] `shouldNotSatisfy` ListPDA.acceptsAngelig pda
        prop "accepts(∃) all strings of length 1" $
          \n -> [n] `shouldSatisfy` ListPDA.acceptsAngelig pda
        prop "rejects(∃) all strings of length >1" $
          \(n, m, w) -> (n : m : w) `shouldNotSatisfy` ListPDA.acceptsAngelig pda
      context "With a PDA with an growing ε-cycle" $ do
        let pda = ListPDA.fromNPDA NPDASpec.npdaEpsilonCycle
        it "rejects(∃) the empty string" $
          [] `shouldNotSatisfy` ListPDA.acceptsAngelig pda
        it "accepts(∃) the string of length 1" $
          [()] `shouldSatisfy` ListPDA.acceptsAngelig pda
        prop "rejects(∃) all strings of length >1" $
          \(w1, w2, ws) ->
            let w = w1 : w2 : ws
             in w `shouldNotSatisfy` ListPDA.acceptsAngelig pda
      context "With a PDA with several growing ε-cycles" $ do
        let pda = ListPDA.fromNPDA NPDASpec.npdaEpsilonCycles
        it "rejects(∃) the empty string" $
          [] `shouldNotSatisfy` ListPDA.acceptsAngelig pda
        prop "accepts(∃) all strings with length >=1 and <=3" $
          \(w1, w2, w3) ->
            let w = maybeToList w1 ++ maybeToList w2 ++ [w3]
             in w `shouldSatisfy` ListPDA.acceptsAngelig pda
        prop "rejects(∃) all strings of length >3" $
          \(w1, w2, w3, w4, ws) ->
            let w = w1 : w2 : w3 : w4 : ws
             in w `shouldNotSatisfy` ListPDA.acceptsAngelig pda
      context "For a NPDA recognizing L = {OᵏIᵏ | k ≥ 0}" $ do
        let (lang, langComp) = (langOkIk, langCompOkIk)
        let pda = ListPDA.fromNPDA NPDASpec.npdaOkIk
        prop "accepts(∃) strings in L" $ do
          (`shouldSatisfy` ListPDA.acceptsAngelig pda) <$> lang
        prop "rejects(∃) strings not in L" $ do
          (`shouldNotSatisfy` ListPDA.acceptsAngelig pda) <$> langComp
      context "For a NPDA recognizing L = {w | w is a palindrome}" $ do
        let (lang, langComp) = (langPalindromes, langCompPalindromes)
        let pda = ListPDA.fromNPDA NPDASpec.npdaPalindromes
        prop "accepts(∃) strings in L" $ do
          (`shouldSatisfy` ListPDA.acceptsAngelig pda) <$> lang
        prop "rejects(∃) strings not in L" $ do
          (`shouldNotSatisfy` ListPDA.acceptsAngelig pda) <$> langComp
      context "For a random NPDA" $
        -- We have to reduce the size a crazy amount,
        -- but otherwise it runs for ages.
        -- Even at this measly size, it has caught a few bugs.
        modifyMaxSize (const 3) $ do
          prop "recognizes the same language" $ do
            \sdpda' w ->
              let snpda = mkNPDA sdpda' :: NPDA.NPDA S A T
                  m = ListPDA.fromNPDA snpda
               in ListPDA.acceptsAngelig m w `shouldBe` NPDA.accepts snpda w
  describe "toNPDA" $ do
    context "For a List PDA recognizing(∃) L = {OᵏIᵏ | k ≥ 0}" $ do
      let (lang, langComp) = (langOkIk, langCompOkIk)
      let snpda = ListPDA.toNPDA pdaOkIk
      prop "accepts strings in L" $ do
        (`shouldSatisfy` NPDA.accepts snpda) <$> lang
      prop "rejects strings not in L" $ do
        (`shouldNotSatisfy` NPDA.accepts snpda) <$> langComp
    context "For a List PDA recognizing(∃) L = {w | w is a palindrome}" $ do
      let (lang, langComp) = (langPalindromes, langCompPalindromes)
      let snpda = ListPDA.toNPDA pdaPalindromes
      prop "accepts strings in L" $ do
        (`shouldSatisfy` NPDA.accepts snpda) <$> lang
      prop "rejects strings not in L" $ do
        (`shouldNotSatisfy` NPDA.accepts snpda) <$> langComp
    context "For a random List PDA" $
      -- We have to reduce the size a crazy amount,
      -- but otherwise it runs for (maybe literally) ages.
      modifyMaxSize (`div` 25) $ do
        prop "recognizes(∃) the same language" $ do
          \m' w ->
            let m = mkMPDA nubOrd m' :: ListPDA.ListPDA R P A T
                sdpda = ListPDA.toNPDA m
             in ListPDA.acceptsAngelig m w `shouldBe` NPDA.accepts sdpda w

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

-- | List PDA that recognizes the EQ language { AⁿBⁿCⁿ | n ≥ 0 }
-- when using *demonic non-determinism*.
--
-- The construction is based on the description in [1, p. 987].
pdaEQ ::
  ListPDA
    (Either (Maybe (NAry 2)) (ABC, NAry 2))
    (Maybe ABC, NAry 2, NAry 3)
    ABC
    (Bottomed ())
pdaEQ =
  MPDA.MonadicPDA
    { MPDA.startSymbol = _startSymbol,
      MPDA.startState = _startState,
      MPDA.finalStates = _finalStates,
      MPDA.transRead = _transRead,
      MPDA.transPop = _transPop
    }
  where
    _startSymbol = Bottom
    _startState = (Left . Just) 1
    _finalStates = Set.map (Left . Just) [1, 2]
    _transRead = \case
      (Left (Just 1), A) ->
        let f x = Right (Nothing, x, undefined)
         in f <$> [1, 2]
      (Right (A, x), A) ->
        pure $ Right (Just A, x, undefined)
      (Right (A, x), B) ->
        pure $ Right (Just B, x, undefined)
      (Right (B, x), B) ->
        pure $ Right (Just B, x, undefined)
      (Right (B, x), C) ->
        pure $ Right (Just C, x, NAry.safeSucc x)
      (Right (C, x), C) ->
        pure $ Right (Just C, x, NAry.safeSucc x)
      _ -> pure $ Left (Left Nothing, [])
    _transPop = \case
      ((Nothing, x, _), Bottom) ->
        pure $ Left (Right (A, x), [Bottom])
      ((Just A, x, _), t) ->
        pure $ Left (Right (A, x), [SSymbol (), t])
      ((Just B, x, _), t) ->
        pure $ Left (Right (B, x), replicate (fromIntegral x) (SSymbol ()) <> [t])
      ((Just C, x, y), SSymbol ()) ->
        case NAry.safePred' y of
          Nothing -> pure $ Left (Right (C, x), [])
          Just y' -> pure $ Right (Just C, x, y')
      ((Just C, _, y), Bottom)
        | y == 1 -> pure $ Left (Left $ Just 2, [Bottom])
        | otherwise -> pure $ Left (Left Nothing, [Bottom])
      c -> error $ "invalid pop state / stack input " ++ show c
