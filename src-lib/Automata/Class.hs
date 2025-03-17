{-# LANGUAGE MultiParamTypeClasses #-}

module Automata.Class where

import Control.Monad (replicateM)
import Data.Universe.Class

-- | Typeclass for automata that act as acceptors of (finite) words.
class Acceptor m a s where
  accepts :: m a s -> [a] -> Bool

-- | A language is a set of strings.
-- We use the list type as this set may be infinite.
type Language a = [[a]]

-- | The language of strings of length `n` that this FA recognizes.
recognizesN :: (Ord a, Finite a, Acceptor m a s) => m a s -> Int -> Language a
recognizesN fa n = filter (accepts fa) ws
  where
    ws = replicateM n universeF

-- | The language that this FA recognizes, grouped by word length.
recognizes :: (Ord a, Finite a, Acceptor m a s) => m a s -> [Language a]
recognizes fa = recognizesN fa <$> [0 ..]
