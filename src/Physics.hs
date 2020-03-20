module Physics(sumVec, dProd, move, translate, sProd, distance, colliding, screenBounce, fVec)
  where

  import Types
  import qualified Graphics.Gloss as Gloss

  fVec :: (Float -> Float -> Float) -> Vector -> Vector -> Vector
  fVec f (x, y) (u, v) = (f x u, f y v)

  sProd :: Float -> Vector -> Vector
  sProd f (x, y) = (f * x, f * y)

  dProd :: Vector -> Vector -> Float
  dProd (ux, uy) (vx, vy) = ux * vx + uy * vy

  sumVec :: Vector -> Vector -> Vector
  sumVec (ux, uy) (vx, vy) = (ux + vx, uy + vy)

  translate :: Vector -> Gloss.Picture -> Gloss.Picture
  translate (x, y) = Gloss.translate x y

  move :: Vector -> Vector -> Picture -> Picture
  move v1 v2 = translate (sumVec v1 v2)

  colliding :: Collider -> Collider -> Bool
  colliding (Crc p1 r1) (Crc p2 r2)                   = distance p1 p2 <= r1 + r2
  colliding (Rec (x1, y1) r1 r2) (Rec (x2, y2) s1 s2) = let p = x1 + r1 >= x2 && x2 + s1 >= x1  
                                                            q = y1 + r2 >= y2 && y2 + s2 >= y1
                                                        in p && q
  colliding (Rec (x, y) s1 s2) (Crc p r)              = colliding (Crc p r) (Rec (x, y) s1 s2)
  colliding (Crc p r) (Rec (x, y) s1 s2)              = False

  distance :: Point -> Point -> Float
  distance (x,y) (u, v) = sqrt ((x + u) * (x + u) + (y + v) * (y + v))

  screenBounce :: Vector -> Vector -> Float -> Float -> Vector
  screenBounce (px, py) (vx, vy) w' h' = (vx', vy')
    where
      h   = h' / 2.0
      w   = w' / 2.0
      vx' = if vx > 0 && px > w || vx < 0 && px < -w then -vx else vx
      vy' = if vy > 0 && py > h || vy < 0 && py < -h then -vy else vy 