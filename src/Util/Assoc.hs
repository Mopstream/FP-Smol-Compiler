module Util.Assoc
  ( Assoc,
    aLookup,
    aDomain,
    aRange,
    aEmpty,
    showAssoc,
  )
where

import Data.List

type Assoc a b = [(a, b)]

aLookup :: (Eq a) => Assoc a b -> a -> b -> b
aLookup ((key', val) : bs) key def
  | key == key' = val
  | otherwise = aLookup bs key def
aLookup [] key def = def

aDomain :: Assoc a b -> [a]
aDomain alist = [key | (key, _) <- alist]

aRange :: Assoc a b -> [b]
aRange alist = [val | (_, val) <- alist]

aEmpty :: Assoc a b
aEmpty = []

showAssoc :: Ord a => Assoc a b -> (a -> String) -> (b -> String) -> String
showAssoc alist fa fb = intercalate "\n" $ map (\(a, b) -> fa a ++ ": " ++ fb b) (sortOn fst alist)
