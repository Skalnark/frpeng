module Facility(sumVec, dProd, translate)
  where

  import Types
  import qualified Graphics.Gloss as Gloss

  sProd :: Float -> Vector -> Vector
  sProd f (x, y) = (f * x, f * y)

  dProd :: Vector -> Vector -> Float
  dProd (ux, uy) (vx, vy) = ux * vx + uy * vy

  sumVec :: Vector -> Vector -> Vector
  sumVec (ux, uy) (vx, vy) = (ux + vx, uy + vy)

  translate :: Vector -> Gloss.Picture -> Gloss.Picture
  translate (x, y) = Gloss.translate x y

  collide :: Collider -> Collider -> Bool
  collide (Circ p1 r1) (Circ p2 r2) = distance p1 p2 <= r1 + r2
  collide _ _ = False

  distance :: Point -> Point -> Float
  distance (x,y) (u, v) = sqrt ((x + u) * (x + u) + (y + v) * (y + v))