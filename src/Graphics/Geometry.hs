module Graphics.Geometry where

import Linear.V3
import Linear.V2

type Triangle = V3 (V3 Double)

isometricProject :: Num a => V3 a -> V2 a
isometricProject = undefined 