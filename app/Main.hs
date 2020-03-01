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

data GS = GS { var :: Values 
             , spr :: [Object]}

type Object = GameEvent -> Gloss.Picture

data GameEvent = GE { kb :: Event Key
                    , gs :: Event GS}

data Values = Val { ballColor :: Gloss.Color
                  , ballSize  :: Float
                  , ballPosition :: Vector
                  , ballVelocity :: Vector
                  }

val :: Values
val = Val { ballColor    = Gloss.blue
          , ballSize     = 25.0
          , ballPosition = (0.0, 0.0)
          , ballVelocity = (10.0, 0.0)
          }

gameState = GS { var = val
               , spr = [ball]
               }

gameEvents = GE { kb = Event keyboard
                , gs = Event gameState}

start = constant gameState

update :: SF GameInput GameEvent
update = (arr (\i -> GE{kb = i, gs = (Event gameState)})) >>> trnslt

render :: SF GameEvent Picture
render = arr $ (\ge -> render' ge)
  where
    render' :: GameEvent -> Picture
    render' ge = Gloss.pictures (map (\x -> x ge) (sp ge))
    sp ge = spr (fromEvent (gs ge))

trnslt :: SF GameEvent GameEvent
trnslt = arr (\(GE k g) -> (GE k (g' g)))
  where
    g' (Event g) = (Event g) `tag` g{var = (tfactor (var g)), spr =  (spr g) }
    tfactor (Val c s (x, y) (v, u)) =  (Val c s (move x  v, move y u) (v, u))
    move p v = p + v


ball :: Object
ball (GE k (Event g)) = translate (ballPosition v) $ Gloss.color (ballColor v) $ Gloss.circleSolid (ballSize v)
  where
    translate (x, y) =  Gloss.translate x y 
    v = var g

main :: IO ()
main = playTheGame gameEvents update render