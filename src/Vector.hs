module Vector (Vec2D(..), (^.), (^*), magnitude, normalize) where

data Vec2D = Vec2D {x :: Double, y :: Double} deriving (Show, Eq)

instance Num Vec2D where
  (Vec2D x1 y1) + (Vec2D x2 y2) = Vec2D (x1 + x2) (y1 + y2)
  (Vec2D x1 y1) - (Vec2D x2 y2) = Vec2D (x1 - x2) (y1 - y2)
  (Vec2D x1 y1) * (Vec2D x2 y2) = Vec2D (x1 * x2) (y1 * y2)
  abs (Vec2D x1 y1) = Vec2D (abs x1) (abs y1)
  signum (Vec2D x1 y1) = Vec2D (signum x1) (signum y1)
  fromInteger n = Vec2D (fromInteger n) (fromInteger n)

dot :: Vec2D -> Vec2D -> Double
dot (Vec2D x1 y1) (Vec2D x2 y2) = x1 * x2 + y1 * y2

(^.) :: Vec2D -> Vec2D -> Double
a ^. b = dot a b
infixl 7 ^.

scale :: Double -> Vec2D -> Vec2D
scale c (Vec2D x1 y1) = Vec2D (c * x1) (c * y1)

(^*) :: Double -> Vec2D -> Vec2D
c ^* v = scale c v
infixl 7 ^*

magnitude :: Vec2D -> Double
magnitude v = sqrt $ dot v v

normalize :: Vec2D -> Vec2D
normalize v =
  let mag = magnitude v
  in if mag == 0 then v
     else Vec2D (x v / mag) (y v / mag)
