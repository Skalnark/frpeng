{-# LANGUAGE Arrows #-}

module Renderer (playTheGame) where

import AdaptedYampaGloss (playYampa)
import Control.Arrow ((>>>))
import FRP.Yampa (Event (..), SF)
import Graphics.Gloss as G
import qualified Graphics.Gloss.Interface.Pure.Game as PG
import Input
import Presets
import Types hiding (Event, SF)

window :: G.Display
window = G.InWindow nameOfTheGame (width, height) (offset, offset)

playTheGame ::
  (gs, Key) ->
  SF (gs, Key) (gs, Key) ->
  (gs -> Picture) ->
  IO ()
playTheGame igs update render = playYampa window background fps mainSF
  where
    applyUpdate ::
      (gs, Key) ->
      SF (gs, Key) (gs, Key) ->
      SF (Event Input) (gs, Key)
    applyUpdate initial update = proc input -> do
      rec cs <- dHold initial -< newGs
          newGs <- Event ^<< update -< newInput
          newInput <- getInput -< (cs, input)
      returnA -< cs

    restart :: SF (gs, Key) (Event (gs, Key))
    restart = arr $ \(gs, key) ->
      if keyESC key == Pressed
        then Event (gs, keyboard)
        else NoEvent

    stateSwitch :: (gs, Key) -> SF (gs, Key) (gs, Key) -> SF (Event Input) (gs, Key)
    stateSwitch i update' =
      switch
        (applyUpdate i update' >>> (identity &&& restart))
        (\_ -> stateSwitch i update')

    mainSF :: SF (Event Input) Picture
    mainSF = stateSwitch igs update >>^ arr fst >>> arr render