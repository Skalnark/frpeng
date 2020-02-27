module Shapes
  ( rectangle
  , solidCircle
  , move
  )
where

import           Graphics.Gloss hiding (Vector)
import           Types

type Width = Float

type Height = Float

solidCircle :: Float -> Picture
solidCircle = circle

rectangle :: Float -> Float -> [Types.Point]
rectangle w h =
  [(-w / 2, h / 2), (w / 2, h / 2), (w / 2, -h / 2), (-w / 2, -h / 2)]


move :: Vector -> Picture -> Picture
move (x, y) p = translate x y p