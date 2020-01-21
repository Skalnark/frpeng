module Main where

import          qualified Graphics.Gloss                     as G
import          Graphics.Gloss.Data.ViewPort       as GV
import         Graphics.Gloss.Interface.Pure.Game as Game

import Primitive
import Shapes

-- | Presets
paddleColor = light (light blue)

width, height, offset :: Int
width = 300

height = 300

offset = 10

fps :: Int
fps = 60

ballSpeed :: Float
ballSpeed = 250

background :: Game.Color
background = black

-- | Data describing the state of the pong game.
data PongGame =
  Game
    { ballLoc :: (Float, Float) -- ^ posição
    , ballVel :: (Float, Float) -- ^ velocidade da bola
    , player1 :: Float -- ^ Player esquerdo
    , player2 :: Float -- ^ Player direito
    }
  deriving (Show) -- | Estado Inicial do pong

moveBall :: Float -> PongGame -> PongGame
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

initialState :: PongGame
initialState =
  Game
    { ballLoc = (-10, 30)
    , ballVel = (ballSpeed * 0.05, ballSpeed * 1)
    , player1 = 40
    , player2 = -80
    }
  -- | Função que converte o estado do jogo em uma imagem

render ::
     PongGame -- ^ Estado do jogo pra renderizar
  -> Game.Picture -- ^ Uma imagem desse estado
render game =
  pictures
    [ ball
    , walls
    , mkPaddle rose 120 $ player1 game
    , mkPaddle orange (-120) $ player2 game
    ]
    -- a bola
  where
    ball = uncurry translate (ballLoc game) $ color ballColor $ circleSolid 10
    ballColor = dark red
  --  as paredes de cima e de baixo
    wall :: Float -> Game.Picture
    wall offset = translate 0 offset $ color wallColor $ rectangleSolid 300 10
    wallColor = greyN 0.5
    walls = pictures [wall 150, wall (-150)]
  --  Faz uma das palhetas
    mkPaddle :: Game.Color -> Float -> Float -> Game.Picture
    mkPaddle col x y =
      pictures
        [ translate x y $ color col $ rectangleSolid 26 86
        , translate x y $ color paddleColor $ rectangleSolid 20 80
        ]

drawing :: Game.Picture
drawing =
  pictures [ball, walls, mkPaddle rose 120 (-20), mkPaddle orange (-120) 40]
  where
    --  A bola
    ball = translate (-10) 40 $ color ballColor $ circleSolid 10
    ballColor = dark red
    --  as paredes
    wall :: Float -> Game.Picture
    wall offset = translate 0 offset $ color wallColor $ rectangleSolid 270 10
    wallColor = greyN 0.5
    walls = pictures [wall 150, wall (-150)]
    --  as palhetas
    mkPaddle :: Game.Color -> Float -> Float -> Game.Picture
    mkPaddle col x y =
      pictures
        [ translate x y $ color col $ rectangleSolid 26 86
        , translate x y $ color paddleColor $ rectangleSolid 20 80
        ]

-- | Atualiza o jogo movendo a bola e implementando o bounce
update :: Float -> PongGame -> PongGame
update seconds = wallBounce . moveBall seconds

-- | Detecta colisão com a palheta e quando colidir altera a
-- velocidade da bola pra simular uma bola quicando
-- paddleBounce :: PongGame -> PongGame
-- | Mesmo que o paddleBounce, com a diferença que esse é em
-- relação a parede
wallBounce :: PongGame -> PongGame
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
wallCollision :: (Float, Float) -> Radius -> Bool
wallCollision (_, y) radius = topCollision || bottomCollision
  where
    topCollision = y - radius <= -fromIntegral width / 2
    bottomCollision = y + radius >= fromIntegral width / 2

-- | Respond to key events.
handleKeys :: Game.Event -> PongGame -> PongGame
-- For an 's' keypress, reset the ball to the center.
handleKeys (Game.EventKey (Game.Char 's') _ _ _) game = game {ballLoc = (0, 0)}
-- Do nothing for all other events.
handleKeys _ game                           = game

main :: IO ()
main = Game.play window background fps initialState render handleKeys update