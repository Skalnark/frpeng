module Presets
  ( fps
  , width
  , height
  , offset
  , nameOfTheGame
  , background
  )
where

import           Graphics.Gloss                as Gloss

nameOfTheGame :: String
nameOfTheGame = "New Game"

width, height, offset, fps :: Int
width = 800
height = 600
offset = 10
fps = 60

background :: Gloss.Color
background = Gloss.black
