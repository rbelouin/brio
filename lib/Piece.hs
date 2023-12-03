module Piece where

import Data.List (nub)
import Geometry

data Direction = L | R deriving (Show, Eq, Ord)

flipped :: Direction -> Direction
flipped L = R
flipped R = L

data EdgeType = End | Innie | Outie deriving (Show, Eq, Ord)
data EdgeShape = Straight { straightLength :: Float }
               | Arc { arcRadius :: Float, arcAngle :: Float, arcDirection :: Direction }
               deriving (Show, Eq, Ord)

data Edge = Edge
  { edgeType :: EdgeType
  , edgeShape :: EdgeShape
  } deriving (Show, Eq, Ord)

data Piece = Piece
  { pieceName :: String
  , pieceBase :: EdgeType
  , edge :: Edge
  } deriving (Show, Eq, Ord)

flipPiece :: Piece -> [Piece]
flipPiece p@(Piece _ _ (Edge _ (Straight _))) = [p]
flipPiece p@(Piece name b (Edge t (Arc r α d))) =
  [ Piece name b (Edge t (Arc r α d))
  , Piece (name ++ "#") b (Edge t (Arc r α $ flipped d))
  ]

rotatePiece :: Piece -> [Piece]
rotatePiece p@(Piece name b (Edge t (Straight l))) = [p, Piece (name ++ "'") t (Edge b (Straight l))]
rotatePiece p@(Piece name b (Edge t (Arc r α d))) = [p, Piece (name ++ "'") t (Edge b (Arc r α $ flipped d))]

permutePiece :: Piece -> [Piece]
permutePiece p = nub $ flipPiece p >>= rotatePiece

matching :: Piece -> Piece -> Bool
matching (Piece _ _ (Edge End _)) _ = False
matching (Piece _ _ (Edge Innie _)) (Piece _ Innie _) = False
matching (Piece _ _ (Edge Outie _)) (Piece _ Outie _) = False
matching _ _ = True

toTransformation :: EdgeShape -> Transformation
toTransformation (Straight l) = Transformation l 0 0
toTransformation (Arc r α d) = Transformation x y β
  where
    x = sin α * r
    y = (if d == R then 1 else -1) * (1 - cos α) * r
    β = if d == R then α else -α

isClosedCombination :: [Piece] -> Bool
isClosedCombination [] = False
isClosedCombination [firstPiece] = False
isClosedCombination pieces@(firstPiece:otherPieces) = matching (last otherPieces) firstPiece && almostEmptyTransformation
  where
    (Transformation x y α) = foldMap (toTransformation . edgeShape . edge) pieces
    -- Let’s have some little tolerance:
    -- 1. Operations on floats can lead to an accumulation of small errors
    -- 2. The BRIO tracks themselves allow for some little flexibility
    almostEmptyTransformation = x < 0.1 && x > -0.1 && y < 0.1 && y > -0.1 && (α < 0.1 || α > 2*pi - 0.1)
