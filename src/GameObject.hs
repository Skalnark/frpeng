module GameObject
  ( initialState
  )
where

import qualified Graphics.Gloss                as Gloss

import           Primitive
import           Presets
import           ObjectAction
import           Renderer
import           Shapes


initialState = [ball, lPaddle, rPaddle, topWall, bottomWall]


-- Ball
ballColor = Gloss.dark Gloss.yellow

wallColor = Gloss.greyN 0.5

ballSize = 15.0

ball = Object
  { renderer = Render
                 { sprite = Gloss.color ballColor $ Gloss.circleSolid ballSize
                 , name   = "Bola"
                 }
  , space    = Space { position = (0, 0), rotation = 0.0, size = (1, 1) }
  , body     = Body { collider = Circle ballSize
                    , mass     = 0.0
                    , velocity = [(45.0, 45.0)]
                    , isSolid  = True
                    }
  , actions  = [translate]
  }

-- Paddles
xSize = 26
ySize = 86

lPaddle = Object
  { renderer = Render
                 { sprite = Gloss.color Gloss.blue
                              $ Gloss.rectangleSolid xSize ySize
                 , name   = "lPlayer"
                 }
  , space    = Space { position = (xSize - fromIntegral width / 2, 0)
                     , rotation = 0.0
                     , size     = (1.0, 1.0)
                     }
  , body     = Body { collider = Polygon (rectangle xSize ySize)
                    , mass     = 0.0
                    , velocity = [(0.0, 0.0)]
                    , isSolid  = True
                    }
  , actions  = []
  }

rPaddle = Object
  { renderer = Render
                 { sprite = Gloss.Color Gloss.red
                              $ Gloss.rectangleSolid xSize ySize
                 , name   = "rPlayer"
                 }
  , space    = Space { position = (fromIntegral width / 2 - xSize, 0)
                     , rotation = 0.0
                     , size     = (1.0, 1.0)
                     }
  , body     = Body { collider = Polygon (rectangle xSize ySize)
                    , mass     = 0.0
                    , velocity = [(0.0, 0.0)]
                    , isSolid  = True
                    }
  , actions  = []
  }

-- Walls

topWall = Object
  { renderer = Render
                 { sprite = Gloss.color Gloss.white
                              $ Gloss.rectangleSolid (fromIntegral width) 10
                 , name   = "topWall"
                 }
  , space    = Space { position = (0, fromIntegral height / 2 - 10)
                     , rotation = 0.0
                     , size     = (1.0, 1.0)
                     }
  , body     = Body { collider = Polygon (rectangle (fromIntegral width) 10)
                    , mass     = 0.0
                    , velocity = [(0.0, 0.0)]
                    , isSolid  = True
                    }
  , actions  = []
  }

bottomWall = Object
  { renderer = Render
                 { sprite = Gloss.color Gloss.white
                              $ Gloss.rectangleSolid (fromIntegral width) 10
                 , name   = "bottomWall"
                 }
  , space    = Space { position = (0, fromIntegral (10 - height) / 2)
                     , rotation = 0.0
                     , size     = (1.0, 1.0)
                     }
  , body     = Body { collider = Polygon (rectangle 10 (fromIntegral width))
                    , mass     = 0.0
                    , velocity = [(0.0, 0.0)]
                    , isSolid  = True
                    }
  , actions  = []
  }
