module ObjectAction
  ( calcVelocity
  , resultantForce
  , translate
  , invert
  )
where

import qualified Graphics.Gloss                as Gloss

import Graphics.Gloss.Interface.Pure.Game hiding (Vector, translate, rotate, scale)

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

translate :: Input -> Float -> Object -> Object
translate input sec (Object r s b bh) = Object { renderer = r
                                               , space    = s'
                                               , body     = b
                                               , behaviors  = bh
                                               }
 where
  s' =
    let p = position s
        v = resultantForce (mass b) (velocity b)
    in  Space { position = sumVec p (scalar sec v)
              , rotation = rotation s
              , size     = size s
              }

invert :: Input -> Float -> Object -> Object
invert (EventKey (SpecialKey KeySpace) Down _ _) sec (Object r s (Body c m v i) bh) =
  Object { renderer = r, space = s, body = b', behaviors = bh }
 where
  b' =
    let nv = scalar (-1) (resultantForce m v)
    in Body{collider  = c, mass = m, velocity = [nv], isSolid = i}
invert _ _ ob = ob
