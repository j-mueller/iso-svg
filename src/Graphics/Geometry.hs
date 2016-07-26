module Graphics.Geometry where

import Linear.Matrix
import Linear.V3
import Linear.V2
import Linear.V1

type Triangle = V3 (V3 Double)

-- | Isometrically project a point onto a 2d plane
isometricProject :: Floating a => V3 a -> V2 a
isometricProject pt = V2 x y where
  V1 (V3 x y _) = (V1 pt) !*! rot 
  rot = (V3 (V3 s3 1 s2) (V3 0 2 ms2) (V3 ms3 1 s2)) !!* fc
  proj = V3 (V3 1 0 0) (V3 0 1 0) (V3 0 0 0)
  s3 = sqrt 3
  s2 = sqrt 2
  ms3 = (-1) * s3
  ms2 = (-1) * s2
  fc = 1 / (sqrt 6)