module Main where

import           System.IO
import           Data.IORef
import           System.Random (newStdGen, StdGen)
import           Control.Arrow
import           Graphics.Gloss.Data.Picture (Picture(Blank))

import           Renderer
import           Shapes
import           ObjectAction
import           Presets
import           GameObject
import           Types
import           Input
import           Physics

data GameState = GameState { bPos   :: !Vector
                           , bVel   :: !Vector
                           , bColor :: Color}

igs :: GameState
igs = GameState { bPos = (0.0, 0.0)
                , bVel = (-5.0, 5.0)
                , bColor = green
}

wall :: Vector -> Color -> Picture
wall p c = translate p $ Color c $ rectangleSolid 900 30

ballRadius = 20.0

ball :: GameState -> Picture
ball gs = translate (bPos gs) $ Color (bColor gs) $ circleSolid ballRadius

ballScript :: SF (GameState, Key) (GameState, Key)
ballScript = arr $ \(gs, key) -> (gs{ bPos = move gs
                                    , bColor = newC (gs, key)
                                    , bVel = bounce gs
                                    }
                                 , key)
  where
    move gs = sumVec (bPos gs) (bVel gs)
    
    newC (gs, key) = case keySPACE key of
                      Released -> red
                      Pressed  -> green
                      _ -> bColor gs

    bounce gs = screenBounce (bPos gs) (bVel gs) (fromIntegral width) (fromIntegral height)

update :: SF (GameState, Key) (GameState, Key)
update = ballScript

render :: GameState -> Picture
render gs = pictures [ ball gs
                     --, wall (0, 300) white
                     --, wall (0, -300) white
                     ]

main = playTheGame (igs, keyboard) update render
