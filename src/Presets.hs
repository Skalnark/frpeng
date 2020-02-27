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

{-
keyboard = Key { keyF1 = IsReleased
               , keyF2 = IsReleased
               , keyF3 = IsReleased
               , keyF4 = IsReleased
               , keyF5 = IsReleased
               , keyF6 = IsReleased
               , keyF7 = IsReleased
               , keyF8 = IsReleased
               , keyF9 = IsReleased
               , keyF10 = IsReleased
               , keyF11 = IsReleased
               , keyF12 = IsReleased
               , keyPAD0 = IsReleased
               , keyPAD1 = IsReleased
               , keyPAD2 = IsReleased
               , keyPAD3 = IsReleased
               , keyPAD4 = IsReleased
               , keyPAD5 = IsReleased
               , keyPAD6 = IsReleased
               , keyPAD7 = IsReleased
               , keyPAD8 = IsReleased
               , keyPAD9 = IsReleased
               , keyMINUS = IsReleased
               , keyPLUS = IsReleased
               , keyTAB  = IsReleased
               , keyESC = IsReleased
               , keyHOME = IsReleased
               , keyPGUP = IsReleased
               , keyPGDOWN = IsReleased
               , keyINSERT = IsReleased
               , keyDELETE = IsReleased
               , keyENTER = IsReleased
               , keyBACKSPACE = IsReleased
               , keyLCTRL = IsReleased
               , keyRCTRL = IsReleased
               , keyLALT = IsReleased
               , keyRALT = IsReleased
               , keyLSHIFT = IsReleased
               , keyRSHIFT = IsReleased
               , keySPACE = IsReleased
               , keyUP = IsReleased
               , keyDOWN = IsReleased
               , keyLEFT = IsReleased
               , keyRIGHT = IsReleased
               , keyA = IsReleased
               , keyB = IsReleased
               , keyC = IsReleased
               , keyD = IsReleased
               , keyE = IsReleased
               , keyF = IsReleased
               , keyG = IsReleased
               , keyH = IsReleased
               , keyI = IsReleased
               , keyJ = IsReleased
               , keyK = IsReleased
               , keyL = IsReleased
               , keyM = IsReleased
               , keyN = IsReleased
               , keyO = IsReleased
               , keyP = IsReleased
               , keyQ = IsReleased
               , keyR = IsReleased
               , keyS = IsReleased
               , keyT = IsReleased
               , keyU = IsReleased
               , keyV = IsReleased
               , keyW = IsReleased
               , keyX = IsReleased
               , keyY = IsReleased
               , keyZ = IsReleased
               , keyLMOUSE = IsReleased
               , keyRMOUSE = IsReleased
               , keyMIDMOUSE = IsReleased
               , keyWHEELUP = IsReleased
               , keyWHEELDOWN = IsReleased
               }
-}