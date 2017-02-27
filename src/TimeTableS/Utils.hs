

module TimeTableS.Utils (
    (<$$>)
  , orderBy
  , concatWithElem
  , forceEither
  , parseInt
  , groupTuples
  ) where

import Data.List
import Data.Char

(<$$>) :: (Functor f1, Functor f) => 
      (a -> b)
  ->  f (f1 a)
  ->  f (f1 b)
(<$$>) f x = fmap (fmap f) x
infixl 5 <$$>

orderBy :: Ord prop => (elem -> prop)  -- ^ selector to be applied
                    -> [elem]          -- ^ to be sorted
                    -> [elem]          -- ^ result
orderBy selector xs = sortBy comparison xs
        where comparison e1 e2 = compare (selector e1) (selector e2)

concatWithElem :: (Monoid m) => m -> [m] -> m
concatWithElem e xs = foldl (\a b -> a `mappend` e `mappend` b) mempty xs

forceEither :: Show e => Either e a -> a
forceEither (Left x) = error (show x)
forceEither (Right x) = x

parseInt :: String -> Maybe Int
parseInt n
    | all isDigit n = Just $ read n
    | otherwise     = Nothing


--TODO: rewrite with clear mind
groupTuples :: Ord c => ((a,b) -> c) -> [(a,b)] -> [(a, [b])]
groupTuples f =
    map (\(a,b) -> (head a,b))
  . map unzip
  . groupBy (\a b -> (f a) == (f b))
  . sortBy  (\a b -> compare (f a) (f b))
