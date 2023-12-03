module Main where

import Inventory
import Piece
import Svg

-- Source: https://woodenrailway.info/track/brio-track-guide
data PieceIdentifier =
  A | A1 | A2 | A3 |
  B | B1 | B2 |
  C | C1 | C2 |
  D |
  E | E1
  deriving (Eq, Show)

toPiece :: PieceIdentifier -> Piece
toPiece identifier = toPiece' identifier
  where
    create = Piece (show identifier)
    toPiece' A = create Innie $ Edge Outie $ Straight 144
    toPiece' A1 = create Innie $ Edge Outie $ Straight 108
    toPiece' A2 = create Innie $ Edge Outie $ Straight 54
    toPiece' A3 = create Innie $ Edge Outie $ Straight 72
    toPiece' B = create Outie $ Edge Outie $ Straight 144
    toPiece' B1 = create Outie $ Edge Outie $ Straight 108
    toPiece' B2 = create Outie $ Edge Outie $ Straight 54
    toPiece' C = create Innie $ Edge Innie $ Straight 144
    toPiece' C1 = create Innie $ Edge Innie $ Straight 108
    toPiece' C2 = create Innie $ Edge Innie $ Straight 54
    toPiece' D = create Innie $ Edge Outie $ Straight 216
    toPiece' E = create Innie $ Edge Outie $ Arc 203 (pi * 2 / 8) R
    toPiece' E1 = create Innie $ Edge Outie $ Arc 110 (pi * 2 / 8) R

inventory :: Inventory Piece
inventory = fromList
  [ (toPiece E, 8)
  , (toPiece A, 4)
  ]

pickEachPermutation :: InventoryPicker Piece Piece
pickEachPermutation = do
  pieceMaybe <- pickEachMaybe
  let pieces = maybe [] permutePiece pieceMaybe
  lift pieces

pickAll :: InventoryPicker Piece [Piece]
pickAll = repeatedlyPickWithFilter matching pickEachPermutation

main :: IO ()
main = sequence_ $ zipWith writeFile filepaths solutions
  where
    filepaths = fmap (\i -> "./dist/output-" ++ show i ++ ".svg") [0..]
    solutions = fmap renderSvg $ filter isClosedCombination $ evalPicker pickAll inventory
