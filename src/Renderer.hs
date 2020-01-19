module Renderer
  ( staticWindow
  ) where

import           Graphics.Gloss as G
import           Shapes

staticWindow :: String -> Size -> Position2D -> Display
staticWindow "" size pos = staticWindow "NewWindow" size pos
staticWindow title (x, y) pos
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
