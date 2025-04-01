module Automata.PushDown.Util (dejavu, split1, split01) where

import Data.Maybe (listToMaybe)

-- | Check if we have been in a similar configuration before.
-- Specifically, check if we have been in a configuration
-- with the same state, same top of the stack, and not a smaller stack.
--
-- This is equivalent to the `existsIn` function from [2].
dejavu :: (Eq s, Eq t, Foldable f) => f (s, [t]) -> (s, [t]) -> Bool
dejavu before now = any (been now) before
  where
    been (s, ts) (q, ys) =
      -- States match,
      s == q
        -- and the top of the stacks match,
        && listToMaybe ts == listToMaybe ys
        -- and the `now` stack has not shrunk.
        && length ts >= length ys

-- | Split a list at the first index,
-- returning the head and tail of the list.
-- If the list is empty, the head is 'Nothing' and the tail is @[]@.
split1 :: [x] -> (Maybe x, [x])
split1 [] = (Nothing, [])
split1 (x : xs) = (Just x, xs)

-- | Split a list at the first *and* zeroth index,
-- returning both the head and tail of the list (as a pair),
-- and the entire list @xs@ (as a pair @('Nothing', xs)@).
--
-- If the list is empty, this is the same as @['split1' xs]@.
split01 :: [x] -> [(Maybe x, [x])]
split01 [] = [(Nothing, [])]
split01 (x : xs) = [(Nothing, x : xs), (Just x, xs)]
