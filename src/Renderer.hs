module Renderer (renderize, update, window) where

import Primitive as P
import Graphics.Gloss as G
import Presets

window :: G.Display
window = G.InWindow nameOfTheGame (width, height) (offset, offset)

renderize :: GameState -> Picture
renderize game = G.pictures $ map P.render game

update :: Float -> GameState -> GameState
update seconds = update' seconds []
 where
  update' sec new []       = new
  update' sec new (o : ob) = update' sec (act sec o : new) ob