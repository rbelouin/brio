module Main where

import Inventory
import Piece

xsEndTrack :: Piece
xsEndTrack = Piece Innie $ Edge End $ Straight (2.0 / 3.0)

xsStraightTrack :: Piece
xsStraightTrack = Piece Innie $ Edge Outie $ Straight 1.0

smStraightTrack :: Piece
smStraightTrack = Piece Innie $ Edge Outie $ Straight 2.0

lgStraightTrack :: Piece
lgStraightTrack = Piece Innie $ Edge Outie $ Straight (8.0 / 3.0)

xlStraightTrack :: Piece
xlStraightTrack = Piece Innie $ Edge Outie $ Straight 4.0

lgRoundedTrack :: Piece
lgRoundedTrack = Piece Innie $ Edge Outie $ Arc (11.0 / 3.0) (pi * 2 / 8) R

inventory :: Inventory Piece
inventory = fromList
  [ (lgRoundedTrack, 8)
  , (smStraightTrack, 4)
  ]

pickEachPermutation :: InventoryPicker Piece Piece
pickEachPermutation = do
  pieceMaybe <- pickEachMaybe
  let pieces = maybe [] permutePiece pieceMaybe
  lift pieces

pickAll :: InventoryPicker Piece [Piece]
pickAll = repeatedlyPickWithFilter matching pickEachPermutation

main :: IO ()
main = print $ length $ filter isClosedCombination $ evalPicker pickAll inventory
