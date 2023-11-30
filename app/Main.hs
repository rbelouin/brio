module Main where

import Control.Monad (mfilter)
import Control.Monad.Extra (fold1M)
import Data.Either (isRight)
import Data.Either.Extra (eitherToMaybe)
import Data.List (nub)
import Data.Maybe (isJust)
import Inventory
import Geometry

data EdgeType = End | Innie | Outie deriving (Show, Eq, Ord)
data EdgeShape = Straight { length :: Float }
               | Arc { arcRadius :: Float, arcAngle :: Float }
               deriving (Show, Eq, Ord)

data Edge = Edge
  { edgeType :: EdgeType
  , edgeShape :: EdgeShape
  } deriving (Show, Eq, Ord)

data Piece = Piece
  { pieceBase :: EdgeType
  , edge :: Edge
  } deriving (Show, Eq, Ord)

data CircuitEdge = CircuitEdge
  { circuitEdgeType :: EdgeType
  , circuitTransformation :: Transformation
  } deriving (Show, Eq)

data Circuit = Circuit
  { circuitBase :: EdgeType
  , circuitEdge :: CircuitEdge
  } deriving (Show, Eq)

fromEdge :: Edge -> CircuitEdge
fromEdge (Edge t (Straight l)) = CircuitEdge t $ Transformation l 0 0
fromEdge (Edge t (Arc r α)) = CircuitEdge t $ Transformation (cos β * r) ((1 + sin β) * r) α
  where β = α - pi / 2

fromPiece :: Piece -> Circuit
fromPiece (Piece b e) = Circuit b $ fromEdge e

concatCircuits :: Circuit -> Circuit -> Either String Circuit
concatCircuits (Circuit _ (CircuitEdge End _ )) _ = Left "Cannot add a piece to an ended circuit"
concatCircuits (Circuit _ (CircuitEdge Innie _)) (Circuit Innie _) = Left "Cannot add an innie to an innie"
concatCircuits (Circuit _ (CircuitEdge Outie _)) (Circuit Outie _) = Left "Cannot add an outie to an outie"
concatCircuits (Circuit b (CircuitEdge _ transformation)) (Circuit _ (CircuitEdge t transformation')) = Right $ Circuit b $ CircuitEdge t $ transformation <> transformation'

matchingPieces :: Piece -> Piece -> Bool
matchingPieces p p' = isRight $ concatCircuits (fromPiece p) (fromPiece p')

isClosed :: Circuit -> Bool
isClosed (Circuit End _) = False
isClosed (Circuit _ (CircuitEdge End _)) = False
isClosed (Circuit Innie (CircuitEdge Innie _)) = False
isClosed (Circuit Outie (CircuitEdge Outie _)) = False
isClosed (Circuit _ (CircuitEdge _ (Transformation x y α))) = x < 0.1 && x > -0.1 && y < 0.1 && y > -0.1 && (β < 0.1 || β > 2*pi - 0.1)
  where
    β = modPi α
    modPi a
      | a < 0 = modPi (a + 2*pi)
      | a >= 2*pi = modPi (a - 2*pi)
      | otherwise = a

flipPiece :: Piece -> [Piece]
flipPiece p@(Piece _ (Edge _ (Straight _))) = [p]
flipPiece p@(Piece b (Edge t (Arc r α))) = [p, Piece b (Edge t (Arc r (-α)))]

rotatePiece :: Piece -> [Piece]
rotatePiece p@(Piece b (Edge t (Straight l))) = [p, Piece t (Edge b (Straight l))]
rotatePiece p@(Piece b (Edge t (Arc r α))) = [p, Piece t (Edge b (Arc r (-α)))]

permutePiece :: Piece -> [Piece]
permutePiece p = nub $ flipPiece p >>= rotatePiece

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
lgRoundedTrack = Piece Innie $ Edge Outie $ Arc (11.0 / 3.0) (pi * 2 / 8)

inventory :: Inventory Piece
inventory = fromList
  [ (lgRoundedTrack, 8)
  , (smStraightTrack, 4)
  ]

isClosed' :: [Piece] -> Bool
isClosed' ps = isJust $ mfilter isClosed $ eitherToMaybe $ fold1M concatCircuits $ fromPiece <$> ps

pickEachPermutation :: InventoryPicker Piece Piece
pickEachPermutation = do
  pieceMaybe <- pickEachMaybe
  let pieces = maybe [] permutePiece pieceMaybe
  lift pieces

pickAll :: InventoryPicker Piece [Piece]
pickAll = repeatedlyPickWithFilter matchingPieces pickEachPermutation

main :: IO ()
main = mapM_ print $ (!! 42) $ filter isClosed' $ evalPicker pickAll inventory
