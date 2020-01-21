module Primitive
  ( Object(..)
  , Render(..)
  , Space(..)
  , Body(..)
  , Collider(..)
  , Sprite
  , Vector
  , Point
  , Force
  , rnd
  , setName
  , setSprite
  , translate
  , rotate
  , resize
  , shear
  , setCollider
  , setVelocity
  , setMass
  , setSolidity
  , calcVelocity
  )
where

import qualified Graphics.Gloss                as Gloss

-- | The main game object
data Object = Object {
  render::Render,
  space::Space,
  body::Body
} deriving (Show, Eq)

apply :: Object -> Object
apply = rnd setName "yay"

type Vector = (Float, Float)

-- Rendering
data Render = Render {
  sprite::Sprite,
  name::String
} deriving (Show, Eq)

rnd :: (p -> Render -> Render) -> p -> Object -> Object
rnd f v (Object r s b) = Object { render = r', space = s, body = b }
  where r' = f v r

setName :: String -> Render -> Render
setName val (Render s _) = Render { sprite = s, name = val }

setSprite :: Sprite -> Render -> Render
setSprite val (Render _ n) = Render { sprite = val, name = n }

-- ^ A point is not a vector
type Point = Vector

type Color = Gloss.Color
type Sprite = Gloss.Picture

-- Transform
data Space = Space{
  position::Vector,
  rotation::Vector,
  size::Vector
} deriving (Show, Eq)

setPosition :: Vector -> Space -> Space
setPosition val (Space _ r s) = Space { position = val, rotation = r, size = s }


translate :: Object -> Float -> Object
translate (Object r space p) sec = Object r s' p
  where
    s' = setPosition (dot sec (position space)) space

rotate :: Vector -> Space -> Space
rotate val (Space p _ s) = Space { position = p, rotation = val, size = s }

shear :: Vector -> Space -> Space
shear (x1, y1) (Space p r (x, y)) = Space { position = p
                                          , rotation = r
                                          , size     = s
                                          }
  where s = (x * x1, y * y1)

resize :: Float -> Space -> Space
resize factor (Space p r (x, y)) = Space { position = p
                                         , rotation = r
                                         , size     = s
                                         }
  where s = (factor * x, factor * y)

-- Physics
data Body = Body {
  collider::Collider,
  mass::Float,
  velocity::Force,
  isSolid::Bool
} deriving (Show, Eq)

setCollider :: Collider -> Body -> Body
setCollider val (Body _ m v i) =
  Body { collider = val, mass = m, velocity = v, isSolid = i }

setMass :: Float -> Body -> Body
setMass val (Body c _ v i) =
  Body { collider = c, mass = val, velocity = v, isSolid = i }

setVelocity :: Force -> Body -> Body
setVelocity val (Body c m _ i) =
  Body { collider = c, mass = m, velocity = val, isSolid = i }

setSolidity :: Bool -> Body -> Body
setSolidity val (Body c m v _) =
  Body { collider = c, mass = m, velocity = v, isSolid = val }

calcVelocity :: Object -> Vector
calcVelocity (Object _ _ b) = calcVelocity' b
  where
    calcVelocity' :: Body -> Vector
    calcVelocity' (Body _ _ v _) = calc v
      where
        calc :: Force -> Vector
        calc []       = (0, 0)
        calc [v     ] = v
        calc (v : vs) = calc' v vs
          where
            calc' :: Vector -> [Vector] -> Vector
            calc' r        []            = r
            calc' (rx, ry) ((x, y) : vs) = calc' (rx + x, ry + y) vs


data Collider = Poligon [Point] | Circle Float | Elipse Vector deriving (Show, Eq)

-- A single object might have multiple forces applied to itself
type Force = [Vector]

dot :: Float -> Vector -> Vector
dot f (x, y) = (f * x, f * y)