module GameObject
  ( initialState
  ) where

import qualified Graphics.Gloss as Gloss

import           ObjectAction
import           Presets
import           Primitive
import           Renderer
import           Shapes

initialState = (zeroF, [ball, lPaddle, rPaddle, topWall, bottomWall])
zeroF :: Float
zeroF = 0
-- Ball
ballColor = Gloss.dark Gloss.yellow

wallColor = Gloss.greyN 0.5

ballSize = 15.0

ball =
  Object
    { sprite = Gloss.color ballColor $ Gloss.circleSolid ballSize
    , name = "Bola"
    , position = (0, 0)
    , collider = Circle ballSize
    , mass = 0.0
    , velocity = (0,0)
    }

-- Paddles
xSize = 26

ySize = 86

lPaddle =
  Object
    { sprite = Gloss.color Gloss.blue $ Gloss.rectangleSolid xSize ySize
    , name = "lPlayer"
    , position = (xSize - fromIntegral width / 2, 0)
    , collider = Polygon (rectangle xSize ySize)
    , mass = 0.0
    , velocity = (0,0)
    }

rPaddle =
  Object
    { sprite = Gloss.color Gloss.red $ Gloss.rectangleSolid xSize ySize
    , name = "lPlayer"
    , position = (fromIntegral width / 2 - xSize, 0)
    , collider = Polygon (rectangle xSize ySize)
    , mass = 0.0
    , velocity = (0,0)
    }

-- Walls
topWall =
  Object
    { sprite =
              Gloss.color Gloss.white $
              Gloss.rectangleSolid (fromIntegral width) 10
    , name = "topWall"
    , position = (0, fromIntegral height / 2 - 10)
    , collider = Polygon (rectangle (fromIntegral width) 10)
    , mass = 0.0
    , velocity = (0,0)
    }

bottomWall =
  Object
    { sprite =
        Gloss.color Gloss.white $
        Gloss.rectangleSolid (fromIntegral width) 10
    , name = "bottomWall"
    , position = (0, fromIntegral (10 - height) / 2)
    , collider = Polygon (rectangle 10 (fromIntegral width))
    , mass = 0.0
    , velocity = (0,0)
    }
