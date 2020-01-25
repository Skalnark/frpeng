module Renderer (renderize, update, window) where

import Primitive as P
import Graphics.Gloss as G
import Presets

window :: G.Display
window = G.InWindow nameOfTheGame (width, height) (offset, offset)

renderize :: GameState -> Picture
renderize game = G.pictures $ map P.render (P.objects game)

update :: Float -> GameState -> GameState
update seconds (input, obj) = update' seconds [] obj
 where
  update' sec new []       = (input, new)
  update' sec new (o : ob) = update' sec (behave input sec o : new) ob