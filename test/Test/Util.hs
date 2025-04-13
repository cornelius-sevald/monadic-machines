{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}

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
import Data.List (uncons)
import Data.Maybe (fromMaybe, maybeToList)
import Data.NAry
import Data.Set (Set)
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
kOkI, nonkOkI :: Gen [Bit]
kOkI = sized $ \n -> do
  k <- chooseInt (0, n `div` 2)
  pure $ replicate k O ++ replicate k I
nonkOkI = mkLangGen (not . p)
  where
    p w =
      let n = length w
          k = n `div` 2
       in even n && all (== O) (take k w) && all (== I) (drop k w)

-- | {w | w contains an even number of 'O's} and its complement.
evenOs, unevenOs :: Gen [Bit]
(evenOs, unevenOs) = mkLangGen' p
  where
    p = even . length . filter (== O)

-- | {w | w ends in 'I'} and its complement.
endsInI, endsNotInI :: Gen [Bit]
(endsInI, endsNotInI) = mkLangGen' p
  where
    p w = not (null w) && last w == I

-- | {w | w contains an 'A's} and its complement.
containsAs, containsNoAs :: Gen [ABC]
containsAs = mkLangGen (A `elem`)
containsNoAs = filter (/= A) <$> arbitrary

-- | {w·c·w^R | c ∉ w}
mirrored, nonmirrored :: Gen [Either ABC ()]
mirrored = do
  w <- fmap (Left <$>) arbitrary
  let c = Right ()
  pure $ w <> [c] <> reverse w
nonmirrored = mkLangGen (not . isMirrored)
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
palindromes, nonpalindromes :: Gen [ABC]
palindromes = do
  w <- arbitrary :: Gen [ABC]
  middle <- arbitrary :: Gen (Maybe ABC)
  pure $ w <> maybeToList middle <> reverse w
nonpalindromes = mkLangGen (not . isPalindrome)
  where
    isPalindrome w = and $ zipWith (==) w (reverse w)

{- Creating automata from arbitrary values. -}

mkDFA :: (s, Set s, Fun (s, a) s) -> DFA.DFA a s
mkDFA (start, final, trans') =
  DFA.DFA
    { DFA.trans = trans,
      DFA.start = start,
      DFA.final = final
    }
  where
    trans = applyFun trans'

mkNFA :: (s, Set s, Fun (s, Maybe a) [s]) -> NFA.NFA a s
mkNFA (start, final, trans') =
  NFA.NFA
    { NFA.trans = trans,
      NFA.start = start,
      NFA.final = final
    }
  where
    trans = applyFun trans'

mkAFA :: (s, Set s, Fun (s, a, Set s) Bool) -> AFA.AFA a s
mkAFA (start, final, trans') =
  AFA.AFA
    { AFA.trans = trans,
      AFA.start = start,
      AFA.final = final
    }
  where
    trans s = uncurry (applyFun3 trans' s)

mkMFA :: (Monad m) => (s, Set s, Fun (s, a) (m s)) -> MFA.MonadicFA a m s
mkMFA (start, final, trans') =
  MFA.MonadicFA
    { MFA.trans = trans,
      MFA.start = start,
      MFA.final = final
    }
  where
    trans = applyFun trans'

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
mkSipserNPDA (startState, finalStates, trans') =
  SNPDA.SipserNPDA
    { SNPDA.startState = startState,
      SNPDA.finalStates = finalStates,
      SNPDA.trans = trans
    }
  where
    trans = applyFun trans'

mkFPDA ::
  ( t,
    r,
    Set r,
    Fun (r, a) p,
    Fun (p, t) (Either (r, [t]) p)
  ) ->
  FPDA.FPDA r p a t
mkFPDA (startSymbol, startState, finalStates, transRead', transPop') =
  FPDA.FPDA
    { FPDA.startSymbol = startSymbol,
      FPDA.startState = startState,
      FPDA.finalStates = finalStates,
      FPDA.transRead = transRead,
      FPDA.transPop = transPop
    }
  where
    transRead = applyFun transRead'
    transPop = applyFun transPop'

-- | Make a monadic PDA.
--
-- The shrink function is intended to make sure
-- the monadic output of the transition function(s) is not needlessly large,
-- e.g. for the List monad it could be 'nubOrd'.
-- If no shrinking is needed, just pass 'id'.
mkMPDA ::
  (Monad m, Ord s, Ord t) =>
  (forall x. (Ord x) => m x -> m x) ->
  (t, s, Set s, Fun (s, t, a) (m (s, [t], Bool)), Fun (s, t) (m s)) ->
  MPDA.MonadicPDA m s a t
mkMPDA shrink (startSymbol, startState, finalStates, δ', γ') =
  MPDA.MonadicPDA
    { MPDA.startSymbol = startSymbol,
      MPDA.startState = startState,
      MPDA.finalStates = finalStates,
      MPDA.transInput = δ,
      MPDA.transStack = γ
    }
  where
    δ = shrink . applyFun δ'
    γ = shrink . applyFun γ'
