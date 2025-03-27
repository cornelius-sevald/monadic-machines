-- | Testing utilities.
module Test.Util where

import Data.Set (Set)
import qualified Automata.FiniteState.AFA as AFA
import qualified Automata.FiniteState.DFA as DFA
import qualified Automata.FiniteState.Monadic as MFA
import qualified Automata.FiniteState.NFA as NFA
import Test.QuickCheck

isqrt :: Int -> Int
isqrt x =
  let y = sqrt $ fromIntegral x :: Double
   in floor y

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
