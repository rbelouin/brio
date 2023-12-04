module Svg where

import Control.Monad.State (State, state, runState)
import Data.Bifunctor (first)
import Geometry
import Piece

data ViewBox = ViewBox Float Float Float Float
data SvgRenderState = SvgRenderState Position ViewBox

emptyRenderState :: SvgRenderState
emptyRenderState = SvgRenderState (Position 0 0 0) $ ViewBox 0 0 0 0

fromPosition :: Position -> SvgRenderState
fromPosition pos = updateWithPosition pos emptyRenderState

updateWithPosition :: Position -> SvgRenderState -> SvgRenderState
updateWithPosition pos@(Position x y _) state@(SvgRenderState _ (ViewBox x' y' x'' y'')) =
  SvgRenderState pos $ ViewBox (min x x') (min y y') (max x x'') (max y y'')

class SvgElement a where
  toElement :: a -> SvgRenderState -> (String, SvgRenderState)

svgString :: SvgElement a => a -> SvgRenderState -> String
svgString elem state = fst $ toElement elem state

nextState :: SvgElement a => a -> SvgRenderState -> SvgRenderState
nextState elem state = snd $ toElement elem state

renderSvg :: SvgElement a => a -> String
renderSvg elem = mconcat
  [ "<svg xmlns=\"http://www.w3.org/2000/svg\" xmlns:xlink=\"http://www.w3.org/1999/xlink\" viewBox=\"" ++ viewBox ++ "\" height=\"" ++ h ++ "\" width=\"" ++ w ++ "\" version=\"1.11.1\">"
  , content
  , "</svg>"
  ]
    where
      (content, SvgRenderState _ (ViewBox x y x' y')) = toElement elem emptyRenderState
      viewBox = show (x - 10) ++ " " ++ show (y - 10) ++ " " ++ w ++ " " ++ h
      w = show $ x' - x + 20
      h = show $ y' - y + 20


instance SvgElement EdgeType where
  toElement End state@(SvgRenderState pos@(Position x y α) _) =
    ("<line x1=\"" ++ x1 ++ "\" y1=\"" ++ y1 ++ "\" x2=\"" ++ x2 ++ "\" y2=\"" ++ y2 ++ "\" />", updateWithPosition pos state)
    where
      x1 = show $ x - δx
      y1 = show $ y + δy
      x2 = show $ x + δx
      y2 = show $ y - δy
      δx = 10 * cos β
      δy = 10 * sin β
      β = (pi / 2) - α
  toElement _ state@(SvgRenderState pos@(Position x y α) _) =
    ("<path d=\"M" ++ p1 ++ " L" ++ p ++ " L" ++ p2 ++ " \"/>", updateWithPosition pos state)
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
  toElement shape@(Straight l) state@(SvgRenderState pos@(Position x y α) _) =
    ("<line x1=\"" ++ x1 ++ "\" y1=\"" ++ y1 ++ "\" x2=\"" ++ x2 ++ "\" y2=\"" ++ y2 ++ "\" />", updateWithPosition pos' state)
    where
      x1 = show x
      y1 = show y
      pos'@(Position x' y' _) = transform (toTransformation shape) pos
      x2 = show x'
      y2 = show y'

  toElement shape@(Arc r β d) state@(SvgRenderState pos@(Position x y α) _) =
    ("<path d=\"M" ++ p1 ++ " A" ++ radiuses ++ " 0.0000 0 " ++ sweepFlag ++ " " ++ p2 ++ " \"/>", updateWithPosition pos' state)
    where
      p1 = show x ++ "," ++ show y
      p2 = show x' ++ "," ++ show y'
      radiuses = show r ++ "," ++ show r
      sweepFlag = if d == R then "1" else "0"
      pos'@(Position x' y' _) = transform (toTransformation shape) pos

instance SvgElement Piece where
  toElement (Piece _ b (Edge t s)) state =
    ("<g>" ++ svgString b state ++ shapeSvg ++ svgString t shapeState ++ "</g>", shapeState)
    where (shapeSvg, shapeState) = toElement s state

instance (SvgElement a) => SvgElement [a] where
  toElement pieces s =
    ("<g stroke=\"black\" stroke-width=\"3\" fill=\"transparent\">" ++ piecesSvg ++ "</g>", piecesState)
    where
      (piecesSvg, piecesState) = first mconcat $ runState elementState s
      elementState :: State SvgRenderState [String]
      elementState = mapM (state . toElement) pieces
