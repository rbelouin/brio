module Inventory where

import Data.Map (Map, toList)
import Data.Tree (Tree(..), Forest)
import GHC.Utils.Misc (holes)

type Inventory k n = Map k n
type FlattenInventory k n = [(k, n)]

permutations :: (Num n, Eq n) => (k -> [k]) -> Inventory k n -> [[k]]
permutations f m = toForest m >>= (paths f)

permutations' :: (Num n, Eq n) => (k -> [k]) -> FlattenInventory k n -> [[k]]
permutations' f kns = toForest' kns >>= (paths f)

pick :: (Num n, Eq n) => FlattenInventory k n -> [(k, FlattenInventory k n)]
pick kns = pick' <$> holes kns

pick' :: (Num n, Eq n) => ((k, n), FlattenInventory k n) -> (k, FlattenInventory k n)
pick' ((k, 1), kns) = (k, kns)
pick' ((k, n), kns) = (k, (k, n-1):kns)

toForest :: (Num n, Eq n) => Inventory k n -> Forest k
toForest = toForest' . toList

toForest' :: (Num n, Eq n) => FlattenInventory k n -> Forest k
toForest' kns = fmap (\(k, kns') -> Node k $ toForest' kns') $ pick kns

paths :: (a -> [a]) -> Tree a -> [[a]]
paths f (Node a []) = (:[]) <$> f a
paths f (Node a as) = (:) <$> f a <*> (as >>= (paths f))
