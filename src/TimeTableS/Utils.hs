module TimeTableS.Utils (
    (<$$>)
  , orderBy
  , concatWithElem
  ) where

import Data.List

fmap2 f = fmap (fmap f)
(<$$>) f x = f `fmap2` x
infixl 4 <$$>

orderBy :: Ord prop => (elem -> prop)  -- ^ selector to be applied
                    -> [elem]          -- ^ to be sorted
                    -> [elem]          -- ^ result
orderBy selector xs = sortBy comparison xs
        where comparison e1 e2 = compare (selector e1) (selector e2)

concatWithElem :: (Monoid m) => m -> [m] -> m
concatWithElem e xs = foldl (\a b -> a `mappend` e `mappend` b) mempty xs
