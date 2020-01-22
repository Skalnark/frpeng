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
  , (===)
  , (<->)
  , (<=>)
  , rd
  , bd
  , sp
  , hasWeight
  ) where

import           GameObject
import qualified Graphics.Gloss as Gloss

type Vector = (Float, Float)

type Point = (Float, Float)

type Color = Gloss.Color

type Sprite = Gloss.Picture

type Force = [Vector]

type Behavior = [Object -> Object]

-- | The main game object
data Object =
  Object
    { render    :: Render
    , space     :: Space
    , body      :: Body
    , behaviors :: Behavior
    }

-- Rendering
data Render =
  Render
    { sprite :: Sprite
    , name   :: String
    }
  deriving (Show, Eq)

-- Transform
data Space =
  Space
    { position :: Vector
    , rotation :: Vector
    , size     :: Vector
    }
  deriving (Show, Eq)

-- Physics
data Body =
  Body
    { collider :: Collider
    , mass     :: Float
    , velocity :: Force
    , isSolid  :: Bool
    }
  deriving (Show, Eq)

data Collider
  = Poligon [Point]
  | Circle Float
  | Elipse Vector
  deriving (Show, Eq)

(<->) :: (p -> Render -> Render) -> p -> Object -> Object
(<->) f v (Object r s b bh) =
  Object {render = r', space = s, body = b, behaviors = bh}
  where
    r' = f v r

rd = (<->)

(<=>) :: (p -> Body -> Body) -> p -> Object -> Object
(<=>) f v (Object r s b bh) =
  Object {render = r, space = s, body = b', behaviors = bh}
  where
    b' = f v b

bd = (<=>)

(===) :: (p -> Space -> Space) -> p -> Object -> Object
(===) f v (Object r s b bh) =
  Object {render = r, space = s', body = b, behaviors = bh}
  where
    s' = f v s

sp = (===)

setName :: String -> Render -> Render
setName val (Render s _) = Render {sprite = s, name = val}

apply = setName <-> "Yay"

setSprite :: Sprite -> Render -> Render
setSprite val (Render _ n) = Render {sprite = val, name = n}

setPosition :: Vector -> Space -> Space
setPosition val (Space _ r s) = Space {position = val, rotation = r, size = s}

translate :: Float -> Object -> Object
translate sec (Object r s b bh) =
  Object {render = r, space = s', body = b, behaviors = bh}
  where
    s' =
      let p = position s
          v = resultantForce (velocity b)
       in Space {position = (vec p v), rotation = rotation s, size = size s}

rotate :: Vector -> Space -> Space
rotate val (Space p _ s) = Space {position = p, rotation = val, size = s}

shear :: Vector -> Space -> Space
shear (x1, y1) (Space p r (x, y)) = Space {position = p, rotation = r, size = s}
  where
    s = (x * x1, y * y1)

resize :: Float -> Space -> Space
resize factor (Space p r (x, y)) = Space {position = p, rotation = r, size = s}
  where
    s = (factor * x, factor * y)

setCollider :: Collider -> Body -> Body
setCollider val (Body _ m v i) =
  Body {collider = val, mass = m, velocity = v, isSolid = i}

setMass :: Float -> Body -> Body
setMass val (Body c _ v i) =
  Body {collider = c, mass = val, velocity = v, isSolid = i}

setVelocity :: Force -> Body -> Body
setVelocity val (Body c m _ i) =
  Body {collider = c, mass = m, velocity = val, isSolid = i}

setSolidity :: Bool -> Body -> Body
setSolidity val (Body c m v _) =
  Body {collider = c, mass = m, velocity = v, isSolid = val}

calcVelocity :: Object -> Vector
calcVelocity (Object _ _ b _) = calcVelocity' b
  where
    calcVelocity' :: Body -> Vector
    calcVelocity' (Body _ _ v _) = resultantForce v

resultantForce :: Force -> Vector
resultantForce [] = (0, 0)
resultantForce [v] = v
resultantForce (v:vs) = resultantForce' v vs
  where
    resultantForce' :: Vector -> [Vector] -> Vector
    resultantForce' r []     = r
    resultantForce' r (v:vs) = resultantForce' (vec r v) vs

hasWeight val (Body c m v i) =
  Body {collider = c, mass = m, velocity = v', isSolid = i}
  where
    v' = ((scalar m val) : v)

scalar :: Float -> Vector -> Vector
scalar f (x, y) = (f * x, f * y)

vec :: Vector -> Vector -> Vector
vec (x1, y1) (x2, y2) = (x1 * x2, y1 * y2)
