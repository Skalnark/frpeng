module Transform where

  import Primitive

  translate :: Position -> Direction -> Position
  translate (px, py) (vx, vy) =  (x, y)
    where
      x = px + vx
      y = py + vy

  -- resize :: Factor -> Size

  -- shear :: Size -> Factor -> Size

  -- rotate :: t -> t
