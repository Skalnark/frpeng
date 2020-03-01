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

--playTheGame :: ge
  --          -> SF GameInput ge
    --        -> SF ge Picture
      --      -> IO ()
playTheGame igs update render = playYampa window background fps mainSF
  where
    --mainSF :: SF (Event Input) Picture
    mainSF = let first = arrPrim (\_ -> igs) &&& getInput 
                 second = (\c -> (constant (updateKeyboard c)) >>> update)
              in render <<< dSwitch first second

-- drpSwitch :: Functor col
--              => (forall sf. a -> col sf -> col (b, sf))
--              -> col (SF b c)
--              -> SF (a, Event (col (SF b c) -> col (SF b c))) (col c)