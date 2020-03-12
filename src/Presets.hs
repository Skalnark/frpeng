module Presets
  ( fps
  , width
  , height
  , offset
  , nameOfTheGame
  , background
  , emptyEvent
  )
where

import           Graphics.Gloss                as Gloss
import Graphics.Gloss.Interface.Pure.Game (Event(EventResize))
import Types

nameOfTheGame :: String
nameOfTheGame = "New Game"

width, height, offset, fps :: Int
width = 800
height = 600
offset = 10
fps = 60

background :: Gloss.Color
background = Gloss.black

emptyEvent = EventResize (fromIntegral width, fromIntegral height)