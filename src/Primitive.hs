module Primitive
  ( Object(..)
  , Render(..)
  , Space(..)
  , Body(..)
  , Collider(..)
  , GameState
  , Behavior
  , Sprite
  , Vector
  , Point
  , Force
  , Input
  , objects
  , inputs
  , render
  , setName
  , setSprite
  , rotate
  , resize
  , shear
  , setCollider
  , setVelocity
  , setMass
  , setSolidity
  , behave
  )
where

import qualified Graphics.Gloss                as Gloss
import qualified Graphics.Gloss.Geometry.Angle as GMath
import qualified Graphics.Gloss.Interface.Pure.Game as Pure

type GameState = (Input, [Object])

type Vector = (Float, Float)

type Point = (Float, Float)

type Color = Gloss.Color

type Sprite = Gloss.Picture

type Input = Pure.Event

type Force = [Vector]

type Behavior = (Input -> Float -> Object -> Object)

-- | The main game object
data Object =
  Object
    { renderer  :: Render
    , space     :: Space
    , body      :: Body
    , behaviors :: [Behavior]
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
    , rotation :: Float
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
  = Polygon [Point]
  | Circle Float
  | Elipse Float Float
  deriving (Show, Eq)


inputs :: GameState -> Input
inputs (e, g) = e

objects :: GameState -> [Object]
objects (e, g) = g

render :: Object -> Sprite
render (Object (Render sp _) (Space (px, py) rot (sx, sy)) b bh) =
  Gloss.translate px py (Gloss.rotate rot (Gloss.scale sx sy sp))

behave :: Input -> Float -> Object -> Object
behave input sec (Object r s bd bh) = behave' input sec bh (Object r s bd bh)
 where
  behave' :: Input -> Float -> [Behavior] -> Object -> Object
  behave' input sec []       obj = obj
  behave' input sec (b : bs) obj = behave' input sec bs (b input sec obj)

setName :: String -> Render -> Render
setName val (Render s _) = Render { sprite = s, name = val }

setSprite :: Sprite -> Render -> Render
setSprite val (Render _ n) = Render { sprite = val, name = n }

setPosition :: Vector -> Space -> Space
setPosition val (Space _ r s) =
  Space { position = val, rotation = r, size = s }

rotate :: Float -> Space -> Space
rotate val (Space p _ s) = Space { position = p, rotation = val, size = s }

shear :: Vector -> Space -> Space
shear (x1, y1) (Space p r (x, y)) = Space { position = p
                                          , rotation = r
                                          , size     = s
                                          }
  where s = (x * x1, y * y1)

resize :: Float -> Space -> Space
resize fbehaveor (Space p r (x, y)) = Space { position = p
                                         , rotation = r
                                         , size     = s
                                         }
  where s = (fbehaveor * x, fbehaveor * y)

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

defaultRender = Render { sprite = Gloss.blank, name = "" }

defaultSpace = Space { position = (0, 0), rotation = 0, size = (1, 1) }

defaultBody =
  Body { collider = Polygon [], mass = 0.0, velocity = [], isSolid = False }
