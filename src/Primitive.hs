module Primitive
  ( Object(..)
  , Render(..)
  , Space(..)
  , Body(..)
  , Collider(..)
  , GameState
  , Action
  , Sprite
  , Vector
  , Point
  , Force
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
  , act
  )
where

import qualified Graphics.Gloss                as Gloss
import qualified Graphics.Gloss.Geometry.Angle as GMath

type GameState = [Object]

type Vector = (Float, Float)

type Point = (Float, Float)

type Color = Gloss.Color

type Sprite = Gloss.Picture

type Force = [Vector]

type Action = (Float -> Object -> Object)

-- | The main game object
data Object =
  Object
    { renderer  :: Render
    , space     :: Space
    , body      :: Body
    , actions :: [Action]
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

render :: Object -> Sprite
render (Object (Render sp _) (Space (px, py) rot (sx, sy)) b bh) =
  Gloss.translate px py (Gloss.rotate rot (Gloss.scale sx sy sp))

act :: Float -> Object -> Object
act sec (Object r s bd bh) = act' sec bh (Object r s bd bh)
 where
  act' :: Float -> [Action] -> Object -> Object
  act' sec []       obj = obj
  act' sec (b : bs) obj = act' sec bs (b sec obj)

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
resize factor (Space p r (x, y)) = Space { position = p
                                         , rotation = r
                                         , size     = s
                                         }
  where s = (factor * x, factor * y)

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
