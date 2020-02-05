{-# LANGUAGE Arrows #-}

module Primitive
  ( Object(..)
  , Collider(..)
  , GameState
  , Sprite
  , Vector
  , Point
  , Input
  , Float
  , render
  )
where

import qualified Graphics.Gloss                as Gloss
import qualified Graphics.Gloss.Geometry.Angle as GMath
import qualified Graphics.Gloss.Interface.Pure.Game as Pure
import Graphics.Gloss.Interface.FRP.Yampa
import FRP.Yampa

type GameState = (Float, [Object])

type Vector = (Float, Float)

type Point = (Float, Float)

type Color = Gloss.Color

type Sprite = Gloss.Picture

type Input = Pure.Event

type Name = String

type Position = Vector
 
type Mass = Float

type Velocity = Vector

data Event a

newtype Behavior a = Behavior(Float -> (a, Behavior a))


-- | The main game object
data Object =
  Object
    { sprite :: Sprite
    , name   :: String
    , position :: Position
    , collider :: Collider
    , mass     :: Mass
    , velocity :: Velocity
    }

data Collider
  = Polygon [Point]
  | Circle Float
  | Elipse Float Float

render :: Object -> Sprite
render (Object s _ (x, y) _ _ _) = Gloss.translate x y s
 