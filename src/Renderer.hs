module Renderer (playTheGame) where

import Presets
import Types hiding (Event, SF)
import Input

import Control.Arrow ((>>>))
import Graphics.Gloss as G
import Graphics.Gloss.Interface.FRP.Yampa (playYampa)
import FRP.Yampa (SF, Event)

window :: G.Display
window = G.InWindow nameOfTheGame (width, height) (offset, offset)

playTheGame :: SF GameInput (GameState e)
    -> SF (GameState e) Picture
    -> IO ()
playTheGame update render = playYampa window background fps mainSF
  where
    mainSF :: SF (Event Input) Picture
    mainSF = getInput >>> update >>> render