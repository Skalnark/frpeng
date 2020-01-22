module Shapes
  ( rectangle
  , solidCircle
  )
where

import           Graphics.Gloss
import           Primitive

type Width = Float

type Height = Float

solidCircle :: Float -> Picture
solidCircle = circle

rectangle :: Float -> Float -> [Primitive.Point]
rectangle w h =
  [(-w / 2, h / 2), (w / 2, h / 2), (w / 2, -h / 2), (-w / 2, -h / 2)]
