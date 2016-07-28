{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Graphics.SceneGraph where

import Control.Lens
import Data.Functor.Fixedpoint
import Linear.Matrix
import Linear.V3

data Triangle a = Triangle (V3 a) (V3 a) (V3 a)
  deriving (Eq, Ord, Show, Functor)

data Geometry a = 
      Mesh [Triangle a]
    | Cube -- unit cube
    deriving (Eq, Ord, Show, Functor)

data Camera a = Camera { _direction :: V3 a, _name :: String }
  deriving (Eq, Ord, Show, Functor)

data SceneGraphF a b =
  EmptyGraph
  | Geo (Geometry a)
  | Group [b]
  | MatrixTransform (M33 a) b -- row-major 
  | Colour String b
  | Light
  | Cam (Camera a) 
  deriving (Eq, Ord, Show, Functor)

mapA :: (a -> c) -> SceneGraphF a b -> SceneGraphF c b
mapA f gr = case gr of
  EmptyGraph -> EmptyGraph
  Geo g -> Geo $ fmap f g
  Group b -> Group b
  MatrixTransform m b -> MatrixTransform (fmap (fmap f) m) b
  Colour s b -> Colour s b
  Light -> Light
  Cam cm -> Cam $ fmap f cm

-- Scene graph type
newtype SceneGraph a = SceneGraph{ _unSceneGraph :: Fix (SceneGraphF a) }

instance Monoid (SceneGraph a) where
    mempty = empty
    mappend l r = case (_unSceneGraph l, _unSceneGraph r) of
      (Fix EmptyGraph, _) -> r
      (_, Fix EmptyGraph) -> l
      (Fix (Group a), Fix (Group b)) -> SceneGraph $ Fix $ Group (a ++ b)
      (Fix (Group a), r') -> SceneGraph $ Fix $ Group (a ++ [r'])
      (l', Fix (Group b)) -> SceneGraph $ Fix $ Group (l':b)
      (l', r') -> SceneGraph $ Fix $ Group [l', r']

instance Functor SceneGraph where
  fmap f (SceneGraph g) = SceneGraph $ hmap (mapA f) g


-- Combinators
cube :: Double -> SceneGraph Double
cube = scale cb where
  cb = SceneGraph $ Fix $ Geo Cube

colour :: SceneGraph a -> String -> SceneGraph a
colour g = SceneGraph . Fix . flip Colour (_unSceneGraph g)

scale :: Num a => SceneGraph a -> a -> SceneGraph a
scale g = SceneGraph . Fix . flip MatrixTransform (_unSceneGraph g) . scaleM

translate :: Num a => SceneGraph a -> V3 a -> SceneGraph a
translate g = SceneGraph . Fix . flip MatrixTransform (_unSceneGraph g) . translateM

empty :: SceneGraph a
empty = SceneGraph $ Fix EmptyGraph  

group :: [SceneGraph a] -> SceneGraph a
group = SceneGraph . Fix . Group . fmap _unSceneGraph

light :: SceneGraph a
light = SceneGraph $ Fix Light

camera :: V3 a -> String -> SceneGraph a
camera dir = SceneGraph . Fix . Cam . Camera dir

-- Matrix transforms

translateM :: Num a => V3 a -> M33 a
translateM t = identity & column _z .~ t

scaleM :: Num a => a -> M33 a
scaleM factor = factor *!! identity