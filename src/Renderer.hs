module Renderer (renderize, update, window) where

import Primitive
import Graphics.Gloss as G
import Presets

window :: G.Display
window = G.InWindow nameOfTheGame (width, height) (offset, offset)

renderize :: GameState -> Picture
renderize (time, obj) = G.pictures $ map Primitive.render obj

update :: Float -> GameState -> GameState
update seconds (time, obj) = (seconds, obj)