module Main where

import qualified Graphics.Gloss                     as G
import qualified Graphics.Gloss.Data.ViewPort       as GV
import qualified Graphics.Gloss.Interface.Pure.Game as Game

import           Primitive                          as P
import           Renderer
import           Shapes

main :: IO ()
main = G.simulate window background fps initialState picture update

sprites game = [change ball, walls, rosePaddle game, orangePaddle game]

data GameState =
  Game
    { ballLoc :: Vector -- ^ posição
    , ballVel :: Vector -- ^ velocidade da bola
    , player1 :: Float -- ^ posição do player esquerdo
    , player2 :: Float -- ^ posição do player direito
    , obj     :: [Object]
    }

gravity :: Vector
gravity = (0.0, -9.8)

rosePaddle game = mkPaddle Game.rose 120 $ player1 game

orangePaddle game = mkPaddle Game.orange (-120) $ player2 game

wall :: Float -> Game.Picture
wall offset =
  Game.translate 0 offset $ Game.color wallColor $ Game.rectangleSolid 300 10

walls = Game.pictures [wall 150, wall (-150)]

ball =
  Object
    { render =
        Render
          { sprite = Game.color ballColor $ Game.circleSolid ballSize
          , name = "Bola"
          }
    , space =
        Space
          {position = (0, 0), rotation = (0, 0), size = (ballSize, ballSize)}
    , body =
        Body
          { collider = Circle ballSize
          , mass = 1.0
          , velocity = [(0.0, 0.0)]
          , isSolid = True
          }
    , behaviors = [(hasWeight <=> gravity)]
    }

--ballMove game = uncurry Game.translate (calcVelocity (body ball))
ballColor = Game.dark Game.red

wallColor = Game.greyN 0.5

ballSize = 10.0

mkPaddle :: Game.Color -> Float -> Float -> Game.Picture
mkPaddle col x y =
  Game.pictures
    [ Game.translate x y $ Game.color col $ Game.rectangleSolid 26 86
    , Game.translate x y $ Game.color paddleColor $ Game.rectangleSolid 20 80
    ]

initialState :: GameState
initialState =
  Game
    { ballLoc = (-10, 30)
    , ballVel = (ballSpeed * 0.05, ballSpeed)
    , player1 = 40
    , player2 = -80
    , obj = []
    }

--------------------------------------------------------------------
change :: Object -> Sprite
change ob = sprite $ render ob

picture game = Game.pictures $ sprites game

drawing :: Game.Picture
drawing =
  Game.pictures
    [ball, walls, mkPaddle Game.rose 120 (-20), mkPaddle Game.orange (-120) 40]
    --  A bola
  where
    ball = Game.translate (-10) 40 $ Game.color ballColor $ Game.circleSolid 10
    ballColor = Game.dark Game.red
  --  as pa Game.redes
    wall :: Float -> Game.Picture
    wall offset =
      Game.translate 0 offset $
      Game.color wallColor $ Game.rectangleSolid 270 10
    walls = Game.pictures [wall 150, wall (-150)]
  --  as palhetas
    mkPaddle :: Game.Color -> Float -> Float -> Game.Picture
    mkPaddle col x y =
      Game.pictures
        [ Game.translate x y $ Game.color col $ Game.rectangleSolid 26 86
        , Game.translate x y $
          Game.color paddleColor $ Game.rectangleSolid 20 80
        ]

-- | Atualiza o jogo movendo a bola e implementando o bounce
update :: GV.ViewPort -> Float -> GameState -> GameState
update _ seconds = wallBounce . moveBall seconds

-- | Detecta colisão com a palheta e quando colidir altera a
-- velocidade da bola pra simular uma bola quicando
-- paddleBounce :: GameState -> GameState
-- | Mesmo que o paddleBounce, com a diferença que esse é em
-- relação a pa Game.rede
wallBounce :: GameState -> GameState
wallBounce game = game {ballVel = (vx, vy')}
    -- O mesmo raio do render
  where
    radius :: Int
    radius = 10
  -- A velocidade anterior em relação a atual
    (vx, vy) = ballVel game
    vy' =
      if wallCollision (ballLoc game) (fromIntegral radius)
                    -- Atualiza a velocidade
        then -vy
                   -- Se não, não faça nada
        else vy

-- | Dada a posição e a colisão da bola, retorna o resultado da colisão
wallCollision :: (Float, Float) -> Float -> Bool
wallCollision (_, y) radius = topCollision || bottomCollision
  where
    topCollision = y - radius <= -fromIntegral width / 2
    bottomCollision = y + radius >= fromIntegral width / 2

-- | Respond to key events.
handleKeys :: Game.Event -> GameState -> GameState
-- For an 's' keypress, reset the ball to the center.
handleKeys (Game.EventKey (Game.Char 's') _ _ _) game = game {ballLoc = (0, 0)}
-- Do nothing for all other events.
handleKeys _ game = game

-- | Presets
paddleColor = Game.light (Game.light Game.blue)

width, height, offset :: Int
width = 300

height = 300

offset = 10

fps :: Int
fps = 60

ballSpeed :: Float
ballSpeed = 250

background :: Game.Color
background = Game.black

moveBall :: Float -> GameState -> GameState
moveBall seconds game = game {ballLoc = (x', y')}
    -- Old locations and velocities.
  where
    (x, y) = ballLoc game
    (vx, vy) = ballVel game
  -- New locations.
    x' = x + vx * seconds
    y' = y + vy * seconds

window :: Game.Display
window = G.InWindow "Window" (width, height) (offset, offset)
