module Primitive
  ( Radius
  , Vector
  , Side
  , Point
  , Color
  , Factor
  , Size
  , Angle
  , Position
  , Mass
  , Direction
  , Force
  ) where

import qualified Graphics.Gloss as Gloss

-- | Multi Purpose
type Radius = Float

type Vector = (Float, Float)

-- Rendering
type Side = Float

-- ^ A point is not a vector
type Point = (Float, Float)

type Color = Gloss.Color

-- Transform
type Factor = Float

type Size = Vector

type Angle = Float

type Position = Vector

-- Physics
type Mass = Float

type Direction = Vector

-- A single object might have multiple forces applied to itself
type Force = [Vector]