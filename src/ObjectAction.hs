module ObjectAction
  ( calcVelocity
  , resultantForce
  , translate
  )
where

import qualified Graphics.Gloss                as Gloss

import           Primitive
import           Presets

gravity :: Vector
gravity = (0, -9.8)

scalar :: Float -> Vector -> Vector
scalar f (x, y) = (f * x, f * y)

vec :: Vector -> Vector -> Vector
vec (x1, y1) (x2, y2) = (x1 * x2, y1 * y2)

dot :: Vector -> Vector -> Float
dot (ux, uy) (vx, vy) = ux * vx + uy * vy

sumVec :: Vector -> Vector -> Vector
sumVec (ux, uy) (vx, vy) = (ux + vx, uy + vy)

calcVelocity :: Object -> Vector
calcVelocity (Object _ _ b _) = calcVelocity' b
 where
  calcVelocity' :: Body -> Vector
  calcVelocity' (Body _ m v _) = resultantForce m v

resultantForce :: Float -> Force -> Vector
resultantForce 0 []       = (0, 0)
resultantForce m []       = scalar m gravity
resultantForce m (v : vs) = resultantForce' v (scalar m gravity : vs)
 where
  resultantForce' :: Vector -> [Vector] -> Vector
  resultantForce' = foldl sumVec

translate :: Float -> Object -> Object
translate sec (Object r s b bh) = Object { renderer = r
                                         , space    = s'
                                         , body     = b
                                         , actions  = bh
                                         }
 where
  s' =
    let p = position s
        v = resultantForce (mass b) (velocity b)
    in  Space { position = sumVec p (scalar sec v)
              , rotation = rotation s
              , size     = size s
              }

