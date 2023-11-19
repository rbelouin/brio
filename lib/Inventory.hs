module Inventory where

import Control.Monad ((<=<))
import Control.Monad.State (StateT(..), get, evalStateT, execStateT)
import qualified Control.Monad.Trans.Class
import qualified Data.Map
import GHC.Utils.Misc (const2, holes)

type Inventory k = Data.Map.Map k Int
type FlattenInventory k = [(k, Int)]
type InventoryPicker k a = StateT (FlattenInventory k) [] a

fromList :: (Ord k) => FlattenInventory k -> Inventory k
fromList = Data.Map.fromList

toList :: (Ord k) => Inventory k -> FlattenInventory k
toList = Data.Map.toList

lift :: [a] -> InventoryPicker k a
lift = Control.Monad.Trans.Class.lift

evalPicker :: (Ord k) => InventoryPicker k a -> Inventory k -> [a]
evalPicker picker inventory = evalStateT picker (toList inventory)

execPicker :: (Ord k) => InventoryPicker k a -> Inventory k -> [Inventory k]
execPicker picker inventory = fromList <$> execStateT picker (toList inventory)

pickEach :: InventoryPicker k k
pickEach = StateT (filterAndPlaceBack <=< holes)
  where
    filterAndPlaceBack :: ((k, Int), FlattenInventory k) -> [(k, FlattenInventory k)]
    filterAndPlaceBack ((_, 0), _) = []
    filterAndPlaceBack ((piece, 1), inventory) = [(piece, inventory)]
    filterAndPlaceBack ((piece, n), inventory) = [(piece, (piece, n-1):inventory)]

pickEachMaybe :: InventoryPicker k (Maybe k)
pickEachMaybe = do
  inventory <- get
  let action | null inventory = return Nothing
             | otherwise = Just <$> pickEach
  action

repeatedlyPick :: InventoryPicker k k -> InventoryPicker k [k]
repeatedlyPick = repeatedlyPickWithFilter (const2 True)

repeatedlyPickWithFilter :: (k -> k -> Bool) -> InventoryPicker k k -> InventoryPicker k [k]
repeatedlyPickWithFilter (=~) picker = do
  inventory <- get
  let action | null inventory = return []
             | otherwise = picker >>= (repeatedlyPickWithFilter' (=~) picker)
  action
    where 
      repeatedlyPickWithFilter' :: (k -> k -> Bool) -> InventoryPicker k k -> k -> InventoryPicker k [k]
      repeatedlyPickWithFilter' (=~) picker piece = do
        inventory <- get
        let action | null inventory = return [piece]
                   | otherwise = picker >>= (\p -> if piece =~ p then (piece:) <$> repeatedlyPickWithFilter' (=~) picker p else lift [])
        action
