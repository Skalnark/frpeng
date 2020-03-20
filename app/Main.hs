module Main where

import           System.IO
import           Data.IORef
import           System.Random (newStdGen, StdGen)
import           Control.Arrow
import           Graphics.Gloss.Data.Picture (Picture(Blank))

import           Renderer
import           Shapes
import           ObjectAction
import           Presets
import           GameObject
import           Types
import           Input
import           Physics

data GameState = GameState { bPos    :: !Vector
                           , bVel    :: !Vector
                           , bColor  :: !Color
                           , p1Pos   :: !Vector
                           , p2Pos   :: !Vector
                           , p1Score :: !Int
                           , p2Score :: !Int
                           , refresh :: !Bool}

igs :: GameState
igs = GameState { bPos = (0.0, 0.0)
                , bVel = (0.0, 0.0)
                , bColor = green
                , p1Pos = (-360.0, 0.0)
                , p2Pos = (360.0, 0.0)
                , p1Score = 0
                , p2Score = 0
                , refresh = False
}

paddleVel :: Vector
paddleVel = (0.0, 10.0)

paddleWidth, paddleHeight, ballRadius:: Float
paddleHeight = 100
paddleWidth = 15
ballRadius = 5.0

player :: Vector -> Color -> Picture
player p c = translate p $ Color c $ rectangleSolid paddleWidth paddleHeight

rendP1, rendP2 :: GameState -> Picture
rendP1 gs = player (p1Pos gs) white
rendP2 gs = player (p2Pos gs) white

controlP1 :: SF (GameState, Key) (GameState, Key)
controlP1 = arr $ \(gs, key) -> let up    = keyW key == Pressed
                                    down  = keyS key == Pressed
                                    limit = 230.0
                                 in if   up && snd (p1Pos gs) <= limit
                                    then (gs{p1Pos = sumVec (p1Pos gs) paddleVel}, key)
                                    else if down && snd (p1Pos gs) >= -limit
                                         then (gs{p1Pos = fVec (-) (p1Pos gs) paddleVel}, key)
                                         else (gs, key)

controlP2 :: SF (GameState, Key) (GameState, Key)
controlP2 = arr $ \(gs, key) -> let up    = keyUP key == Pressed
                                    down  = keyDOWN key == Pressed
                                    limit = 230.0
                                 in if   up && snd (p2Pos gs) <= limit
                                    then (gs{p2Pos = sumVec (p2Pos gs) paddleVel}, key)
                                    else if down && snd (p2Pos gs) >= -limit
                                         then (gs{p2Pos = fVec (-) (p2Pos gs) paddleVel}, key)
                                         else (gs, key)

wall :: Vector -> Color -> Picture
wall p c = translate p $ Color c $ rectangleSolid 900 30

ball :: GameState -> Picture
ball gs = translate (bPos gs) $ Color (bColor gs) $ circleSolid (ballRadius * 2.0)

ballScript :: SF (GameState, Key) (GameState, Key)
ballScript = arr $ \(gs, key) -> (gs{ bPos = move gs}, key)
  where
    move gs = sumVec (bPos gs) (bVel gs)

    newC (gs, key) = case keySPACE key of
                      Released -> red
                      Pressed  -> green
                      _ -> bColor gs

bouncePaddle :: (GameState, Key) -> (GameState, Key)
bouncePaddle (gs, key) = 
   let x1       = fst $ p1Pos gs
       y1       = snd $ p1Pos gs
       x2       = fst $ p2Pos gs
       y2       = snd $ p2Pos gs
       w        = paddleWidth / 2.0
       h        = paddleHeight / 2.0
       x        = ballRadius + fst (bPos gs)
       y        = snd $ bPos gs
       (v1, v2) = bVel gs
       c1  = y <= y1 + h && y >= y1 - h && x1 <= x && x1 + w >= x
       c2  = y <= y2 + h && y >= y2 - h && x2 >= x && x2 - w <= x
       v   = if c1 || c2 then (-v1, v2) else (v1, v2)
    in (gs{bVel = v}, key)

bounceBall :: SF (GameState, Key) (GameState, Key)
bounceBall = arr $ \(gs, key) ->
  ( gs{bVel = screenBounce (bPos gs)
                           (bVel gs)
                           (fromIntegral width)
                           (fromIntegral height - 60)}
  , key)

reset :: SF (GameState, Key) (GameState, Key)
reset = arr $ \(gs, key) -> if refresh gs 
                            then (gs{ bPos = (0.0, 0.0)
                                    , refresh = False
                                    }
                                 , key)
                            else (gs, key)

score :: Int -> SF (GameState, Key) (GameState, Key)
score w' = arr $ \(gs, key) -> 
   let x = fst $ bPos gs
       p1 = p1Score gs
       p2 = p2Score gs
       w  = fromIntegral w' / 2.0  
    in if x < -w
       then (gs{p2Score = p2 + 1, refresh = True}, key)
       else if x > w
             then (gs{p1Score = p1 + 1, refresh = True}, key)
             else (gs, key)

drawScore :: GameState -> Picture
drawScore gs = let p1 = p1Score gs
                   p2 = p2Score gs
                in pictures [ translate (-200, 120) $ scale 0.7 0.7 $ Color white $ Text (show p1)
                            , translate (150, 120) $ scale 0.7 0.7 $ Color white $ Text (show p2)]

update :: SF (GameState, Key) (GameState, Key)
update =     ballScript 
         >>> bounceBall 
         >>> bouncePaddle 
         ^>> controlP1 
         >>> controlP2
         >>> score width
         >>> reset

render :: GameState -> Picture
render gs = pictures [ ball gs
                     , wall (0, 300) white
                     , wall (0, -300) white
                     , rendP1 gs
                     , rendP2 gs
                     , drawScore gs
                     ]

main = playTheGame (igs, keyboard) update render
