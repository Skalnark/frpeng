module Main where

import qualified Graphics.Gloss                as Gloss
import qualified Graphics.Gloss.Interface.Pure.Game
                                               as PureGame

import           Renderer
import           Shapes
import           ObjectAction
import           Presets
import           GameObject
import           Primitive

main :: IO ()
main = PureGame.play window background fps initialState renderize events update

events :: PureGame.Event -> GameState -> GameState
events _ g' = g'
