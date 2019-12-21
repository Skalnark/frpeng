module Main where

import Graphics.Gloss as G
import Renderer
import Shapes

-- | Window and color preset
paddleColor = light (light blue)

width, height, offset :: Int
width = 300

height = 300

offset = 10

background :: Color
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

initialState :: PongGame
initialState =
  Game {ballLoc = (-10, 30), ballVel = (1, -3), player1 = 40, player2 = -80}
  -- | Função que converte o estado do jogo em uma imagem

render ::
     PongGame -- ^ Estado do jogo pra renderizar
  -> Picture -- ^ Uma imagem desse estado
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
    wall :: Float -> Picture
    wall offset = translate 0 offset $ color wallColor $ rectangleSolid 270 10
    wallColor = greyN 0.5
    walls = pictures [wall 150, wall (-150)]
  --  Faz uma das palhetas
    mkPaddle :: Color -> Float -> Float -> Picture
    mkPaddle col x y =
      pictures
        [ translate x y $ color col $ rectangleSolid 26 86
        , translate x y $ color paddleColor $ rectangleSolid 20 80
        ]

drawing :: Picture
drawing =
  pictures [ball, walls, mkPaddle rose 120 (-20), mkPaddle orange (-120) 40]
  where
    --  A bola
    ball = translate (-10) 40 $ color ballColor $ circleSolid 10
    ballColor = dark red
    --  as paredes
    wall :: Float -> Picture
    wall offset = translate 0 offset $ color wallColor $ rectangleSolid 270 10
    wallColor = greyN 0.5
    walls = pictures [wall 150, wall (-150)]
    --  as palhetas
    mkPaddle :: Color -> Float -> Float -> Picture
    mkPaddle col x y =
      pictures
        [ translate x y $ color col $ rectangleSolid 26 86
        , translate x y $ color paddleColor $ rectangleSolid 20 80
        ]

main :: IO ()
main = do
  G.display
    (window "Window" (width, height) (offset, offset))
    background
    drawing
  print "yay"
