module Main where

import           System.IO
import           Data.IORef
import           Control.Arrow
import qualified Graphics.Gloss                as Gloss
import Graphics.Gloss.Data.Picture (Picture(Blank))
import qualified Graphics.Gloss.Interface.Pure.Game
                                               as PureGame
import qualified Graphics.Gloss.Interface.FRP.Yampa as FYampa
import FRP.Yampa

import           Renderer
import           Shapes
import           ObjectAction
import           Presets
import           GameObject
import           Primitive

main :: IO ()
main = FYampa.playYampa window background fps blankImage
--main = PureGame.play window background fps initialState renderize events update

blankImage :: SF (Event FYampa.InputEvent) Picture
blankImage = constant Blank

events :: Input -> GameState -> GameState
events _ game = game
