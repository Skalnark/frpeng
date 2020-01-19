module Shapes
  ( rectangle
  , solidCircle
  , Radius
  , Width
  , Height
  , Size
  , Position
  , Position2D
  , Position3D
  ) where

import           Graphics.Gloss

type Radius = Float

type Width = Float

type Height = Float

type Size = (Int, Int)

type Position3D = (Int, Int, Int)

type Position2D = (Int, Int)

data Position
  = Position3D
  | Position2D

solidCircle :: Radius -> Picture
solidCircle f = circle f

rectangle :: Width -> Height -> Picture
rectangle w h =
  polygon [(-w / 2, h / 2), (w / 2, h / 2), (w / 2, -h / 2), (-w / 2, -h / 2)]
