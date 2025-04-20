module Data.OrdFunctor where

import qualified Data.Set as Set

class OrdFunctor f where
  ordFmap :: (Ord a, Ord b) => (a -> b) -> f a -> f b

instance OrdFunctor [] where
  ordFmap = map

instance OrdFunctor Set.Set where
  ordFmap = Set.map
