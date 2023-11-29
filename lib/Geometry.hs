{-# LANGUAGE NumericUnderscores #-}

module Geometry where

data Transformation = Transformation Float Float Float deriving (Eq)

instance Semigroup Transformation where
  (Transformation x y α) <> (Transformation x' y' β) = Transformation (x + x'') (y + y'') (α + β)
    where (x'', y'') = rotate x' y' α

instance Monoid Transformation where
  mempty = Transformation 0 0 0

instance Show Transformation where
  show (Transformation x y α) = "T(→" ++ show x ++ ", ↑" ++ show y ++ ",  ⃔" ++ show α ++ ")"

data Position = Position Float Float Float deriving (Eq)

instance Show Position where
  show (Position x y α) = "P(x=" ++ show x ++ ", y=" ++ show y ++ ", α=" ++ show α ++ ")"

transform :: Transformation -> Position -> Position
transform t (Position x y α) = Position x' y' β
  where (Transformation x' y' β) = Transformation x y α <> t

(~>) :: Position -> Transformation -> Position
(~>) p t = transform t p

canonical :: Position -> Position
canonical (Position x y α) = Position (Geometry.round x) (Geometry.round y) (Geometry.round $ canonicalAngle α)
  where
    canonicalAngle β
      | β < 0 = canonicalAngle (β + 2 * pi)
      | β >= (2 * pi) = canonicalAngle (β - 2 * pi)
      | otherwise = β

round :: Float -> Float
round x = fromIntegral (Prelude.round $ x * 10_000) / 10_000

rotate :: Float -> Float -> Float -> (Float, Float)
rotate x y α = fromPolar r (α + θ)
  where (r, θ) = toPolar x y

toPolar :: Float -> Float -> (Float, Float)
toPolar 0 0 = (0, 0) -- any angle could be used, but we’ll use 0 for simplicity
toPolar x y = (r, θ)
  where
    r = sqrt $ x*x + y*y
    θ = (if y >= 0 then 1 else -1) * acos (x / r)

fromPolar :: Float -> Float -> (Float, Float)
fromPolar r θ = (r * cos θ, r * sin θ)
