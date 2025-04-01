-- | Testing utilities.
module Test.Util where

import qualified Automata.FiniteState.AFA as AFA
import qualified Automata.FiniteState.DFA as DFA
import qualified Automata.FiniteState.Monadic as MFA
import qualified Automata.FiniteState.NFA as NFA
import Data.Alphabet
import Data.Maybe (maybeToList)
import Data.Set (Set)
import Test.QuickCheck

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

-- | {w | w is a palindrome} and its complement.
palindromes, nonpalindromes :: Gen [ABC]
palindromes = do
  w <- arbitrary :: Gen [ABC]
  middle <- arbitrary :: Gen (Maybe ABC)
  pure $ w <> maybeToList middle <> reverse w
nonpalindromes = mkLangGen (not . palindrome)
  where
    palindrome w = and $ zipWith (==) w (reverse w)

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
