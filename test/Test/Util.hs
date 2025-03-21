-- | Testing utilities.
module Test.Util where

import qualified Automata.AFA as AFA
import qualified Automata.DFA as DFA
import qualified Automata.Monadic as MFA
import qualified Automata.NFA as NFA
import Data.Set (Set)
import Test.QuickCheck

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

mkMFA :: (Monad m) => (s, Set s, Fun (s, a) (m s)) -> MFA.AutomatonM a m s
mkMFA (start, final, trans') =
  MFA.AutomatonM
    { MFA.trans = trans,
      MFA.start = start,
      MFA.final = final
    }
  where
    trans = applyFun trans'
