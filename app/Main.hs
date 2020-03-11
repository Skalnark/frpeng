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
import           Facility

data GameState = GameState { bPos   :: !Vector
                           , bVel   :: !Vector
                           , bcolor :: Color}

igs = GameState { bPos = (0.0, 0.0)
                , bVel = (0.5, 0.0)
                , bcolor = green}

wall :: Vector -> Color -> Picture
wall p c = translate p $ Color c $ rectangleSolid 900 30

ball :: GameState -> Picture
ball gs = trans (bPos gs) (bVel gs) $ Color (bcolor gs) $ circleSolid 20.0
  where
    trans p v= translate (sumVec p v)

moveBall :: (GameState, Key) -> (GameState, Key)
moveBall (gs, key) = (gs{bPos = move' gs}, key)
  where
    move' (GameState p v b) = sumVec p v

ballColor :: (GameState, Key) -> (GameState, Key)
ballColor (gs, key) = case keySPACE key of
                      IsReleased -> (gs{bcolor = red}, key)
                      IsPressed  -> (gs{bcolor = green}, key)
                      _ -> (gs, key)

update :: (GameState, Key) -> (GameState, Key)
update (gs, key) = moveBall $ ballColor (gs, key)

render :: GameState -> Picture
render gs = pictures [ball gs, wall (0, 300) white, wall (0, -300) white]

main = playTheGame (igs, keyboard) update render
