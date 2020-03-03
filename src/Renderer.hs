module Renderer (playTheGame) where

import Presets
import Types hiding (Event, SF)
import Input

import Control.Arrow ((>>>))
import Graphics.Gloss as G
import Graphics.Gloss.Interface.FRP.Yampa (playYampa)
import FRP.Yampa (SF, Event(..))

window :: G.Display
window = G.InWindow nameOfTheGame (width, height) (offset, offset)

playTheGame :: ge
            -> SF GameInput ge
            -> SF ge Picture
            -> IO ()
playTheGame igs update render = playYampa window background fps mainSF
  where
    mainSF :: SF (Event Input) Picture
    mainSF = render <<< s <<< i
    i      = getInput &&& now update
    s      = rSwitch update
-- rSwitch :: SF GameInput ge
--         -> SF (GameInput, Event (SF GameInput ge)) ge

-- rSwitch :: SF in out
--         -> SF (in, Event (SF in out)) out

{-
    mainSF = let first  = arrPrim (\_ -> igs) &&& getInput 
                 second c = constant (updateKeyboard c) >>> update
              in render <<< switch first second

--SF GameInput (ge, Event Key) -> (Key -> SF GameInput ge) -> SF GameInput ge
--SF a (b, Event c) -> (c -> SF a b) -> SF a b
-}

input :: SF (Event Key, GameInput) GameInput
input = arr $ \((Event k), keys) -> (updateKeyboard k keys)