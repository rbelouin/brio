module Main where

import Control.Monad (foldM)

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
  , circuitEdgeX :: Float
  , circuitEdgeY :: Float
  , circuitEdgeAngle :: Float
  } deriving (Show, Eq)

data Circuit = Circuit
  { circuitBase :: EdgeType
  , circuitEdge :: CircuitEdge
  } deriving (Show, Eq)

fromEdge :: Edge -> CircuitEdge
fromEdge (Edge t (Straight l)) = CircuitEdge t l 0 0
fromEdge (Edge t (Arc r α)) = CircuitEdge t ((cos β) * r) ((1 + sin β) * r) α
  where β = α - pi / 2

fromPiece :: Piece -> Circuit
fromPiece (Piece b e) = Circuit b $ fromEdge e

fromPolar :: Floating a => a -> a -> (a, a)
fromPolar magnitude angle = (magnitude * cos angle, magnitude * sin angle)

toPolar :: Floating a => a -> a -> (a, a)
toPolar x y = (magnitude, angle)
  where
    magnitude = sqrt $ x*x + y*y
    angle = acos $ x / magnitude

rotate :: Floating a => a -> a -> a -> (a, a)
rotate x y α = fromPolar m (α + β)
  where (m, β) = toPolar x y

concatCircuits :: Circuit -> Circuit -> Either String Circuit
concatCircuits (Circuit _ (CircuitEdge End _ _ _)) _ = Left "Cannot add a piece to an ended circuit"
concatCircuits (Circuit _ (CircuitEdge Innie _ _ _)) (Circuit Innie _) = Left "Cannot add an innie to an innie"
concatCircuits (Circuit _ (CircuitEdge Outie _ _ _)) (Circuit Outie _) = Left "Cannot add an outie to an outie"
concatCircuits (Circuit b (CircuitEdge _ x y α)) (Circuit _ (CircuitEdge t x' y' β)) = Right $ Circuit b $ CircuitEdge t (x + x'') (y + y'') (α + β)
  where (x'', y'') = rotate x' y' α

isClosed :: Circuit -> Bool
isClosed (Circuit End _) = False
isClosed (Circuit _ (CircuitEdge End _ _ _)) = False
isClosed (Circuit Innie (CircuitEdge Innie _ _ _)) = False
isClosed (Circuit Outie (CircuitEdge Outie _ _ _)) = False
isClosed (Circuit _ (CircuitEdge _ x y α)) = x < 0.1 && x > -0.1 && y < 0.1 && y > -0.1 && (β < 0.1 || β > 2*pi - 0.1)
  where
    β = modPi α
    modPi a = if a < 0 then modPi (a + 2*pi) else if (a >= 2*pi) then modPi (a - 2*pi) else a

xsEndTrack :: Piece
xsEndTrack = Piece Innie $ Edge End $ Straight (2.0 / 3.0)

xsStraightTrack :: Piece
xsStraightTrack = Piece Innie $ Edge Outie $ Straight 1.0

smStraightTrack :: Piece
smStraightTrack = Piece Innie $ Edge Outie $ Straight 2.0

lgStraightTrack :: Piece
lgStraightTrack = Piece Innie $ Edge Outie $ Straight (8.0 / 3.0)

xlStraightTrack :: Piece
xlStraightTrack = Piece Innie $ Edge Outie $ Straight (4.0)

lgRoundedTrack :: Piece
lgRoundedTrack = Piece Innie $ Edge Outie $ Arc (11.0 / 3.0) (pi * 2 / 8) 

main :: IO ()
main = putStrLn $ show $ fmap isClosed $ foldM concatCircuits (fromPiece lgRoundedTrack) $ fromPiece <$> (replicate 7 lgRoundedTrack)
