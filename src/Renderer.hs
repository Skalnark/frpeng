module Renderer
  ( window
  ) where

import Graphics.Gloss as G

type Size = (Int, Int)

type Position3D = (Int, Int, Int)

type Position2D = (Int, Int)

window :: String -> Size -> Position2D -> Display
window "" size pos = window "NewWindow" size pos
window title (x, y) pos
  | x <= 0 || y <= 0 = renderExact' title (100, 100) pos
  | otherwise = renderExact' title (x, y) pos
  where
    renderExact' :: String -> Size -> Position2D -> Display
    renderExact' title size (w, h)
      | w <= 0 || h <= 0 = renderExact'' title size (100, 100)
      | otherwise = renderExact'' title size (w, h)
      where
        renderExact'' :: String -> Size -> Position2D -> Display
        renderExact'' title size pos = G.InWindow title size pos
