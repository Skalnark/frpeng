module Shapes
  ( rectangle
  , solidCircle
  ) where

import Graphics.Gloss

type Radius = Float

type Width = Float

type Height = Float

solidCircle :: Radius -> Picture
solidCircle f = circle f

rectangle :: Width -> Height -> Picture
rectangle w h =
  polygon [(-w / 2, h / 2), (w / 2, h / 2), (w / 2, -h / 2), (-w / 2, -h / 2)]
