module Main where

import           System.IO
import           Data.IORef
--import System.Random (newStdGen, StdGen)
import           Control.Arrow
import qualified Graphics.Gloss                as Gloss
import Graphics.Gloss.Data.Picture (Picture(Blank))
import qualified Graphics.Gloss.Interface.Pure.Game
                                               as PureGame hiding (Event)

import           Renderer
import           Shapes
import           ObjectAction
import           Presets
import           GameObject
import           Types
import           Input

type GS = GameState Values
type Object = GS -> Gloss.Picture

data Values = Val { ballColor :: Gloss.Color
                  , ballSize  :: Float
                  , ballPosition :: Vector
                  , ballVelocity :: Vector
                  }

ev :: Event Values
ev = Event Val  { ballColor = Gloss.blue
                , ballSize  = 25.0
                , ballPosition = (0.0, 0.0)
                , ballVelocity = (0.0, 0.0)
                }

gameState = GameState { var     = ev
                      , sprites = [ball]
                      }


update :: SF GameInput GS
update = arr (const gameState)

render :: SF GS Picture
render = arr render'
  where
    render' :: GS -> Picture
    render' gs = Gloss.pictures (map (\x -> x gs) (sprites gs))

ball :: Object
ball (GameState (Event v) _ ) = move (ballPosition v) $ Gloss.color (ballColor v) $ Gloss.circleSolid (ballSize v)

main :: IO ()
main = playTheGame update render