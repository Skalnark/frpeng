module Main where

import           System.IO
import           Data.IORef
import           Control.Monad                  ( forever
                                                , when
                                                )
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



events :: Input -> GameState -> GameState
events newInput (_, obj) = (newInput, obj)
