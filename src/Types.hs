module Types
  ( Collider(..)
  , Sprite
  , Vector
  , Point
  , GameInput(..)
  , Key(..)
  , KeyState(..)
  , Input
  , module FRP.Yampa
  , Event(..)
  )
where

import qualified Graphics.Gloss                     as Gloss
import qualified Graphics.Gloss.Geometry.Angle      as GMath
import qualified Graphics.Gloss.Interface.Pure.Game as Pure
import qualified Graphics.Gloss.Interface.FRP.Yampa as YampaI
import FRP.Yampa

type Vector = (Float, Float)

type Point = (Float, Float)

type Color = Gloss.Color

type Sprite = Gloss.Picture

type Input = YampaI.Input

type GameObject input output = SF input output

type ObjectState s = s

data Collider
  = Polygon [Point]
  | Circle Float
  | Elipse Float Float

type GameInput = Event Key

data KeyState = IsPressed | IsReleased | KeyDown | KeyUp | None

data Key = Key { keyF1 :: KeyState
               , keyF2 :: KeyState
               , keyF3 :: KeyState
               , keyF4 :: KeyState
               , keyF5 :: KeyState
               , keyF6 :: KeyState
               , keyF7 :: KeyState
               , keyF8 :: KeyState
               , keyF9 :: KeyState
               , keyF10 :: KeyState
               , keyF11 :: KeyState
               , keyF12 :: KeyState
               , keyPAD0 :: KeyState
               , keyPAD1 :: KeyState
               , keyPAD2 :: KeyState
               , keyPAD3 :: KeyState
               , keyPAD4 :: KeyState
               , keyPAD5 :: KeyState
               , keyPAD6 :: KeyState
               , keyPAD7 :: KeyState
               , keyPAD8 :: KeyState
               , keyPAD9 :: KeyState
               , keyMINUS :: KeyState
               , keyPLUS :: KeyState
               , keyTAB :: KeyState
               , keyESC :: KeyState
               , keyHOME :: KeyState
               , keyPGUP :: KeyState
               , keyPGDOWN :: KeyState
               , keyINSERT :: KeyState
               , keyDELETE :: KeyState
               , keyENTER :: KeyState
               , keyBACKSPACE :: KeyState
               , keyLCTRL :: KeyState
               , keyRCTRL :: KeyState
               , keyLALT :: KeyState
               , keyRALT :: KeyState
               , keyLSHIFT :: KeyState
               , keyRSHIFT :: KeyState
               , keySPACE :: KeyState
               , keyUP :: KeyState
               , keyDOWN :: KeyState
               , keyLEFT :: KeyState
               , keyRIGHT :: KeyState
               , keyA :: KeyState
               , keyB :: KeyState
               , keyC :: KeyState
               , keyD :: KeyState
               , keyE :: KeyState
               , keyF :: KeyState
               , keyG :: KeyState
               , keyH :: KeyState
               , keyI :: KeyState
               , keyJ :: KeyState
               , keyK :: KeyState
               , keyL :: KeyState
               , keyM :: KeyState
               , keyN :: KeyState
               , keyO :: KeyState
               , keyP :: KeyState
               , keyQ :: KeyState
               , keyR :: KeyState
               , keyS :: KeyState
               , keyT :: KeyState
               , keyU :: KeyState
               , keyV :: KeyState
               , keyW :: KeyState
               , keyX :: KeyState
               , keyY :: KeyState
               , keyZ :: KeyState
               , keyLMOUSE :: KeyState
               , keyRMOUSE :: KeyState
               , keyMIDMOUSE :: KeyState
               , keyWHEELUP :: KeyState
               , keyWHEELDOWN :: KeyState
               }
