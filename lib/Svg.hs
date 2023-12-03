module Svg where

import Control.Monad.State (State, state, runState)
import Data.Bifunctor (first)
import Geometry
import Piece

class SvgElement a where
  toElement :: a -> Position -> (String, Position)

svgString :: SvgElement a => a -> Position -> String
svgString elem pos = fst $ toElement elem pos

nextPosition :: SvgElement a => a -> Position -> Position
nextPosition elem pos = snd $ toElement elem pos

renderSvg :: SvgElement a => a -> String
renderSvg elem = mconcat
  -- TODO: stop hardcoding the size of the viewBox
  [ "<svg xmlns=\"http://www.w3.org/2000/svg\" xmlns:xlink=\"http://www.w3.org/1999/xlink\" viewBox=\"-1000 -1000 2000 2000\" height=\"1000\" width=\"1000\" version=\"1.11.1\">"
  , svgString elem $ Position 0 0 0
  , "</svg>"
  ]

instance SvgElement EdgeType where
  toElement End pos@(Position x y α) =
    ("<line x1=\"" ++ x1 ++ "\" y1=\"" ++ y1 ++ "\" x2=\"" ++ x2 ++ "\" y2=\"" ++ y2 ++ "\" />", pos)
    where
      x1 = show $ x - δx
      y1 = show $ y + δy
      x2 = show $ x + δx
      y2 = show $ y - δy
      δx = 10 * cos β
      δy = 10 * sin β
      β = (pi / 2) - α
  toElement _ pos@(Position x y α) =
    ("<path d=\"M" ++ p1 ++ " L" ++ p ++ " L" ++ p2 ++ " \"/>", pos)
    where
      p = show x ++ "," ++ show y
      p1 = x1 ++ "," ++ y1
      p2 = x2 ++ "," ++ y2
      x1 = show $ x + 10 * cos β
      y1 = show $ y - 10 * sin β
      x2 = show $ x + 10 * cos (β + pi / 2)
      y2 = show $ y - 10 * sin (β + pi / 2)
      β = (3 * pi / 4) - α

instance SvgElement EdgeShape where
  toElement shape@(Straight l) pos@(Position x y α) =
    ("<line x1=\"" ++ x1 ++ "\" y1=\"" ++ y1 ++ "\" x2=\"" ++ x2 ++ "\" y2=\"" ++ y2 ++ "\" />", pos')
    where
      x1 = show x
      y1 = show y
      pos'@(Position x' y' _) = transform (toTransformation shape) pos
      x2 = show x'
      y2 = show y'

  toElement shape@(Arc r β d) pos@(Position x y α) =
    ("<path d=\"M" ++ p1 ++ " A" ++ radiuses ++ " 0.0000 0 " ++ sweepFlag ++ " " ++ p2 ++ " \"/>", pos')
    where
      p1 = show x ++ "," ++ show y
      p2 = show x' ++ "," ++ show y'
      radiuses = show r ++ "," ++ show r
      sweepFlag = if d == R then "1" else "0"
      pos'@(Position x' y' _) = transform (toTransformation shape) pos

instance SvgElement Piece where
  toElement (Piece _ b (Edge t s)) pos =
    ("<g>" ++ svgString b pos ++ shapeSvg ++ svgString t shapePosition ++ "</g>", shapePosition)
    where (shapeSvg, shapePosition) = toElement s pos

instance (SvgElement a) => SvgElement [a] where
  toElement pieces pos =
    ("<g stroke=\"black\" stroke-width=\"3\" fill=\"transparent\">" ++ piecesSvg ++ "</g>", piecesPosition)
    where
      (piecesSvg, piecesPosition) = first mconcat $ runState elementState pos
      elementState :: State Position [String]
      elementState = mapM (state . toElement) pieces
