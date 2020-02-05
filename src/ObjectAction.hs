module ObjectAction
where

import qualified Graphics.Gloss                as Gloss

import Graphics.Gloss.Interface.Pure.Game hiding (Vector, translate, rotate, scale)

import           Primitive
import           Presets

scalar :: Float -> Vector -> Vector
scalar f (x, y) = (f * x, f * y)

vec :: Vector -> Vector -> Vector
vec (x1, y1) (x2, y2) = (x1 * x2, y1 * y2)

dot :: Vector -> Vector -> Float
dot (ux, uy) (vx, vy) = ux * vx + uy * vy

sumVec :: Vector -> Vector -> Vector
sumVec (ux, uy) (vx, vy) = (ux + vx, uy + vy)