module Main where

import           System.IO
import           Data.IORef
import           System.Random (newStdGen, StdGen)
import           Control.Arrow
import qualified Graphics.Gloss                as Gloss
import           Graphics.Gloss.Data.Picture (Picture(Blank))

import           Renderer
import           Shapes
import           ObjectAction
import           Presets
import           GameObject
import           Types
import           Input

data GameState = GameState { keys :: Key
                           , bPos :: Vector
                           , bVel :: Vector}

type Object = GE -> Gloss.Picture

type GE = Event GameState

gs = Event GameState{keys = keyboard, bPos = (0.0, 0.0), bVel = (100.0, 70.0) }

render :: SF GE Picture
render = arr ball

update :: SF GameInput GE
update = (arr $ \gi -> gs) >>> move

move :: SF GE GE
move = arr (\(Event ge) -> (Event ge) `tag` ge{bPos = (move' (bPos ge) (bVel ge))})
  where
    move' (x, y) (vx, vy) = (x + vx, y + vy)

ball:: Object
ball = \(Event (GameState k p v)) -> trans p $ Gloss.Color Gloss.red $ Gloss.circle 20.0
  where
    trans (x, y) = Gloss.translate x y

main = playTheGame gs update render
