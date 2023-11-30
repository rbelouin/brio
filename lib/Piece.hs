module Piece where

import Data.List (nub)
import Geometry

data EdgeType = End | Innie | Outie deriving (Show, Eq, Ord)
data Direction = L | R deriving (Show, Eq, Ord)
data EdgeShape = Straight { straightLength :: Float }
               | Arc { arcRadius :: Float, arcAngle :: Float, arcDirection :: Direction }
               deriving (Show, Eq, Ord)

data Edge = Edge
  { edgeType :: EdgeType
  , edgeShape :: EdgeShape
  } deriving (Show, Eq, Ord)

data Piece = Piece
  { pieceBase :: EdgeType
  , edge :: Edge
  } deriving (Show, Eq, Ord)

flipPiece :: Piece -> [Piece]
flipPiece p@(Piece _ (Edge _ (Straight _))) = [p]
flipPiece p@(Piece b (Edge t (Arc r α _))) =
  [ Piece b (Edge t (Arc r α L))
  , Piece b (Edge t (Arc r α R))
  ]

rotatePiece :: Piece -> [Piece]
rotatePiece p@(Piece b (Edge t (Straight l))) = [p, Piece t (Edge b (Straight l))]
rotatePiece p@(Piece b (Edge t (Arc r α d))) = [p, Piece t (Edge b (Arc r α (if d == L then R else L)))]

permutePiece :: Piece -> [Piece]
permutePiece p = nub $ flipPiece p >>= rotatePiece

matching :: Piece -> Piece -> Bool
matching (Piece _ (Edge End _)) _ = False
matching (Piece _ (Edge Innie _)) (Piece Innie _) = False
matching (Piece _ (Edge Outie _)) (Piece Outie _) = False
matching _ _ = True

toTransformation :: Piece -> Transformation
toTransformation (Piece _ (Edge e (Straight l))) = Transformation (l * 20) 0 0
toTransformation (Piece _ (Edge e (Arc r α d))) = Transformation x y β
  where
    x = sin α * r * 20
    y = (if d == R then 1 else -1) * (1 - cos α) * r * 20
    β = if d == R then α else -α

isClosedCombination :: [Piece] -> Bool
isClosedCombination [] = False
isClosedCombination [firstPiece] = False
isClosedCombination pieces@(firstPiece:otherPieces) = matching (last otherPieces) firstPiece && almostEmptyTransformation
  where
    (Transformation x y α) = foldMap toTransformation pieces
    -- Let’s have some little tolerance:
    -- 1. Operations on floats can lead to an accumulation of small errors
    -- 2. The BRIO tracks themselves allow for some little flexibility
    almostEmptyTransformation = x < 0.1 && x > -0.1 && y < 0.1 && y > -0.1 && (α < 0.1 || α > 2*pi - 0.1)
