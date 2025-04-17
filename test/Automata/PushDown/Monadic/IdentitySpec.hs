{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLists #-}

module Automata.PushDown.Monadic.IdentitySpec where

import qualified Automata.PushDown.FPDASpec as FPDASpec
import Automata.PushDown.Monadic.Identity (IdentityPDA)
import qualified Automata.PushDown.Monadic.Identity as IdentityPDA
import qualified Automata.PushDown.SipserDPDA as SDPDA
import qualified Automata.PushDown.SipserDPDASpec as SDPDASpec
import Automata.PushDown.Util (Bottomed)
import Data.Alphabet
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.Util

spec :: Spec
spec = do
  describe "Example Identity PDAs" $ do
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
  describe "fromSipserDPDA" $ do
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
  describe "toSipserDPDA" $ do
    context "For a DPDA recognizing L = {OᵏIᵏ | k ≥ 0}" $ do
      let (lang, langComp) = (kOkI, nonkOkI)
      let sdpda = IdentityPDA.toSipserDPDA pdakOkI
      prop "accepts strings in L" $ do
        (`shouldSatisfy` SDPDA.accepts sdpda) <$> lang
      prop "rejects strings not in L" $ do
        (`shouldNotSatisfy` SDPDA.accepts sdpda) <$> langComp
    context "For a DPDA recognizing L = {w·c·w^R | c ∉ w}" $ do
      let (lang, langComp) = (mirrored, nonmirrored)
      let sdpda = IdentityPDA.toSipserDPDA pdaMirrored
      prop "accepts strings in L" $ do
        (`shouldSatisfy` SDPDA.accepts sdpda) <$> lang
      prop "rejects strings not in L" $ do
        (`shouldNotSatisfy` SDPDA.accepts sdpda) <$> langComp

{- Example Identity PDAs -}

-- | A PDA which recognizes the language {OᵏIᵏ | k ≥ 0}.
--
-- See 'FPDASpec.fpdakOkI'.
pdakOkI :: IdentityPDA (Maybe Int) () Bit Char
pdakOkI = IdentityPDA.fromFPDA FPDASpec.fpdakOkI

-- | A PDA which recognizes the language {w·c·w^R | c ∉ w}.
--
-- See 'FPDASpec.fpdaMirrored.
pdaMirrored :: IdentityPDA (Either Bool ABC) Int (Either ABC ()) (Bottomed ABC)
pdaMirrored = IdentityPDA.fromFPDA FPDASpec.fpdaMirrored
