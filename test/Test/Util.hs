{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Testing utilities.
module Test.Util where

import qualified Automata.FiniteState.AFA as AFA
import qualified Automata.FiniteState.DFA as DFA
import qualified Automata.FiniteState.Monadic as MFA
import qualified Automata.FiniteState.NFA as NFA
import qualified Automata.PushDown.FPDA as FPDA
import qualified Automata.PushDown.Monadic as MPDA
import qualified Automata.PushDown.SipserDPDA as SDPDA
import qualified Automata.PushDown.SipserNPDA as SNPDA
import Control.Monad (guard)
import Data.Alphabet
import Data.Either (isLeft)
import Data.List (genericReplicate, uncons)
import qualified Data.Logic.NormalForm as NF
import Data.Maybe (fromJust, fromMaybe, maybeToList)
import Data.NAry
import Data.OrdFunctor (OrdFunctor (ordFmap))
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Universe.Class (Finite (..))
import Test.QuickCheck hiding (shrink)

{- Utility functions -}

isqrt :: Int -> Int
isqrt x =
  let y = sqrt $ fromIntegral x :: Double
   in floor y

-- | Make a generator for a language from a given predicate.
mkLangGen :: (Arbitrary a) => ([a] -> Bool) -> Gen [a]
mkLangGen p = arbitrary `suchThat` p

-- | Make a generator for a language and its complement
-- from a given predicate.
mkLangGen' :: (Arbitrary a) => ([a] -> Bool) -> (Gen [a], Gen [a])
mkLangGen' p = (mkLangGen p, mkLangGen (not . p))

{- Example languages -}

-- | {OᵏIᵏ | k ≥ 0} and its complement.
langOkIk, langCompOkIk :: Gen [Bit]
langOkIk = sized $ \n -> do
  k <- chooseInt (0, n `div` 2)
  pure $ replicate k O ++ replicate k I
langCompOkIk = mkLangGen (not . p)
  where
    p w =
      let n = length w
          k = n `div` 2
       in even n && all (== O) (take k w) && all (== I) (drop k w)

-- | {w | w contains an even number of 'O's} and its complement.
langEvenOs, langCompEvenOs :: Gen [Bit]
(langEvenOs, langCompEvenOs) = mkLangGen' p
  where
    p = even . length . filter (== O)

-- | {w | w ends in 'I'} and its complement.
langEndsInI, langCompEndsInI :: Gen [Bit]
(langEndsInI, langCompEndsInI) = mkLangGen' p
  where
    p w = not (null w) && last w == I

-- | {w | w contains an 'A's} and its complement.
langContainsAs, langCompContainsAs :: Gen [ABC]
langContainsAs = mkLangGen (A `elem`)
langCompContainsAs = filter (/= A) <$> arbitrary

-- | {w·c·w^R | c ∉ w}
langMirrored, langCompMirrored :: forall a. (Eq a, Arbitrary a) => Gen [Either a ()]
langMirrored = do
  w <- fmap (Left <$>) arbitrary
  let c = Right ()
  pure $ w <> [c] <> reverse w
langCompMirrored = mkLangGen (not . isMirrored)
  where
    isMirrored w = fromMaybe False $ do
      let n = length w `div` 2
          wl = take n w
      (c, wr) <- uncons $ drop n w
      guard $ all isLeft wl
      guard $ c == Right ()
      guard $ all isLeft wr
      pure $ wl == reverse wr

-- | {w | w is a palindrome} and its complement.
langPalindromes, langCompPalindromes :: forall a. (Eq a, Arbitrary a) => Gen [a]
langPalindromes = do
  w <- arbitrary :: Gen [a]
  middle <- arbitrary :: Gen (Maybe a)
  pure $ w <> maybeToList middle <> reverse w
langCompPalindromes = mkLangGen (not . isPalindrome)
  where
    isPalindrome w = and $ zipWith (==) w (reverse w)

-- | { w·w } and its complement.
--
-- What's special about this languag is that it is not in CFL,
-- but its complement is.
langRepeated, langCompRepeated :: forall a. (Eq a, Arbitrary a) => Gen [a]
langRepeated = do
  w <- arbitrary :: Gen [a]
  pure $ w <> w
langCompRepeated = mkLangGen (not . repeated)
  where
    repeated w =
      let n = length w
       in even n && and (zipWith (==) w $ drop (n `div` 2) w)

-- | { AⁿBⁿCⁿ | n ≥ 0 } and its complement.
--
-- This language is not in CFL, but its complement is.
langEQ, langCompEQ :: Gen [ABC]
langEQ = do
  n <- getNonNegative <$> arbitrary :: Gen Integer
  let as = genericReplicate n A
      bs = genericReplicate n B
      cs = genericReplicate n C
  pure $ as <> bs <> cs
langCompEQ = oneof [gen1, gen2]
  where
    -- Generator for *any* string not in EQ.
    gen1 = mkLangGen (not . eq)
    -- Strings of the form { A^i B^j C^j | ¬(i = j = k) }
    gen2 = do
      (i, j, k) <- arbitrary `suchThat` (\(i, j, k) -> (i /= j) || (i /= k))
      let as = genericReplicate (getNonNegative i :: Integer) A
          bs = genericReplicate (getNonNegative j) B
          cs = genericReplicate (getNonNegative k) C
      pure $ as <> bs <> cs
    eq w =
      let k = length w
          (n, r) = k `quotRem` 3
          as = take n w
          bs = take n $ drop n w
          cs = drop (2 * n) w
       in r == 0
            && all (== A) as
            && all (== B) bs
            && all (== C) cs

-- | { x#y | x ∈ EQ, y ∈ comp(EQ) } and its complement.
--
-- As EQ is not in CFL, then this language is clearly
-- neither in CFL or co-CFL.
langEQNonEQ, langCompEQNonEQ :: Gen [Maybe ABC]
langEQNonEQ = do
  l <- langEQ
  lComp <- langCompEQ
  pure $ fmap Just l <> [Nothing] <> fmap Just lComp
langCompEQNonEQ = gen
  where
    -- Strings of the form { x#y } where (x, y) are members of
    -- one of { EQ × EQ, comp(EQ) × EQ, comp(EQ) × comp(EQ) },
    -- chosen at random.
    gen = do
      (b1, b2) <- arbitrary `suchThat` (\(b1, b2) -> not (b1 && not b2))
      l1 <- if b1 then langEQ else langCompEQ
      l2 <- if b2 then langEQ else langCompEQ
      pure $ fmap Just l1 <> [Nothing] <> fmap Just l2

lang3SAT, langComp3SAT :: Int -> Gen [NF.Literal Integer]
(lang3SAT, langComp3SAT) = (lang, langComp)
  where
    langEither n = do
      let normalizeAtoms p =
            let assoc = zip (Set.toList $ NF.cnfAtoms p) [1 ..]
             in ordFmap (fromJust . flip lookup assoc) p
      cnf' <- arbitrary `suchThat` (\x -> length x <= n) :: Gen (NF.CNF Integer)
      let cnf = normalizeAtoms cnf'
      let cnf3 = NF.to3CNF cnf
      let side = if NF.cnfSatisifiable cnf then Left else Right
      pure $ side $ concat cnf3
    lang n = suchThatMap (langEither n) (either Just (const Nothing))
    langComp n = suchThatMap (langEither n) (either (const Nothing) Just)

{- Creating automata from arbitrary values. -}

mkDFA :: (s, Set s, Fun (s, a) s) -> DFA.DFA a s
mkDFA (start, final, trans) =
  DFA.DFA
    { DFA.start = start,
      DFA.final = final,
      DFA.trans = applyFun trans
    }

mkNFA :: (s, Set s, Fun (s, Maybe a) [s]) -> NFA.NFA a s
mkNFA (start, final, trans) =
  NFA.NFA
    { NFA.start = start,
      NFA.final = final,
      NFA.trans = applyFun trans
    }

mkAFA :: (Finite s, Ord s) => (s, Set s, Fun (s, a, Set s) Bool) -> AFA.AFA a s
mkAFA (start, final, trans') =
  AFA.AFA
    { AFA.start = start,
      AFA.final = final,
      AFA.trans = trans
    }
  where
    trans (q, a) u = applyFun3 trans' q a (AFA.indicate u)

mkMFA :: (Monad m) => (s, Set s, Fun (s, a) (m s)) -> MFA.MonadicFA a m s
mkMFA (start, final, trans) =
  MFA.MonadicFA
    { MFA.start = start,
      MFA.final = final,
      MFA.trans = applyFun trans
    }

-- Here we can't just generate any transition function,
-- as we need to ensure the condition that exactly one of
-- δ(s, ε, ε), δ(s, ε, t), δ(s, a, ε), δ(s, a, t) is defined.
-- To do this, we generate an additional selection function,
-- which maps each state to a number between 1 and 4,
-- indicating which of the four options for δ is to be defined.
mkSipserDPDA ::
  (s, Set s, Fun s (NAry 4), Fun (s, Maybe t, Maybe a) (s, [t])) ->
  SDPDA.SipserDPDA s a t
mkSipserDPDA (startStates, finalStates, select', trans') =
  SDPDA.SipserDPDA
    { SDPDA.startState = startStates,
      SDPDA.finalStates = finalStates,
      SDPDA.trans = trans
    }
  where
    select = applyFun select'
    δ = applyFun trans'
    trans c@(s, _, _) = case (select s, c) of
      (Ith 1, (_, Nothing, Nothing)) -> Just $ δ c
      (Ith 2, (_, Nothing, Just _)) -> Just $ δ c
      (Ith 3, (_, Just _, Nothing)) -> Just $ δ c
      (Ith 4, (_, Just _, Just _)) -> Just $ δ c
      _ -> Nothing

mkSipserNPDA ::
  (s, Set s, Fun (s, Maybe t, Maybe a) (Set (s, [t]))) ->
  SNPDA.SipserNPDA s a t
mkSipserNPDA (startState, finalStates, trans) =
  SNPDA.SipserNPDA
    { SNPDA.startState = startState,
      SNPDA.finalStates = finalStates,
      SNPDA.trans = applyFun trans
    }

mkFPDA ::
  ( t,
    r,
    Set r,
    Fun (r, a) (Either (r, [t]) p),
    Fun (p, t) (Either (r, [t]) p)
  ) ->
  FPDA.FPDA r p a t
mkFPDA (startSymbol, startState, finalStates, transRead, transPop) =
  FPDA.FPDA
    { FPDA.startSymbol = startSymbol,
      FPDA.startState = startState,
      FPDA.finalStates = finalStates,
      FPDA.transRead = applyFun transRead,
      FPDA.transPop = applyFun transPop
    }

-- | Make a monadic PDA.
--
-- The shrink function is intended to make sure
-- the monadic output of the transition function(s) is not needlessly large,
-- e.g. for the List monad it could be 'nubOrd'.
-- If no shrinking is needed, just pass 'id'.
mkMPDA ::
  (Monad m) =>
  (m (Either (r, [t]) p) -> m (Either (r, [t]) p)) ->
  ( t,
    r,
    Set r,
    Fun (r, a) (m (Either (r, [t]) p)),
    Fun (p, t) (m (Either (r, [t]) p))
  ) ->
  MPDA.MonadicPDA m r p a t
mkMPDA shrink (startSymbol, startState, finalStates, transRead, transPop) =
  MPDA.MonadicPDA
    { MPDA.startSymbol = startSymbol,
      MPDA.startState = startState,
      MPDA.finalStates = finalStates,
      MPDA.transRead = shrink . applyFun transRead,
      MPDA.transPop = shrink . applyFun transPop
    }
