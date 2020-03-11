{-# LANGUAGE Arrows #-}
module Renderer (playTheGame) where

import Presets
import Types hiding (Event, SF)
import Input

import Control.Arrow ((>>>))
import Graphics.Gloss as G
import qualified Graphics.Gloss.Interface.Pure.Game as PG

import Graphics.Gloss.Interface.FRP.Yampa (playYampa)
import FRP.Yampa (SF, Event(..))

window :: G.Display
window = G.InWindow nameOfTheGame (width, height) (offset, offset)

playTheGame :: (gs, Key)
            -> ((gs, Key) -> (gs, Key))
            -> (gs -> Picture)
            -> IO ()
playTheGame igs update render = playYampa window background fps mainSF
  where
    mainLoop :: (gs, Key)
             -> ((gs, Key) -> (gs, Key))
             -> SF (Event Input) (gs, Key)
    mainLoop state update = proc input -> do
      rec currentState <- dHold state -< gameUpdated
          gameUpdated <- arr $ Event . update -< (getInput (updateKey currentState) input)
      returnA -< currentState

    endGame :: SF (gs, Key) (Event (gs, Key))
    endGame = arr $  \(gs, key) -> if keyESC key == IsPressed
                              then Event (gs, keyboard)
                              else NoEvent
    game i  = dSwitch (mainLoop i update >>> (identity &&& endGame))
                       (\_ -> mainLoop i update)
    mainSF :: SF (Event Input) Picture
    mainSF  = game igs >>> arr fst >>> arr render