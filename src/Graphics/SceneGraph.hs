{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
module Graphics.SceneGraph where

import Control.Lens
import Data.Functor.Fixedpoint
import Data.Monoid
import Linear.Matrix
import Linear.V3
import Linear.V4

data Triangle a = Triangle {
  _p1 :: V3 a,
  _p2 :: V3 a,
  _p3 :: V3 a,
  _normal :: V3 a,
  _colour :: String
}
  deriving (Eq, Ord, Show, Functor)

makeLenses ''Triangle

data Geometry a = 
      Mesh [Triangle a]
    | Cube -- unit cube
    deriving (Eq, Ord, Show, Functor)

makePrisms ''Geometry

toTriangles :: (Fractional a, Num a) => Geometry a -> [Triangle a]
toTriangles g = case g of
  Mesh ts -> ts
  Cube -> top <> bottom <> left <> right <> front <> back where
      bl = "black"
      n = (- 0.5)
      p = 0.5
      top = [
        Triangle (V3 n p n) (V3 n p p) (V3 p p n) (V3 n p n) bl, 
        Triangle (V3 n p p) (V3 p p n) (V3 p p p) (V3 n p n) bl]
      bottom = [
        Triangle (V3 n n n) (V3 n n p) (V3 p n n) (V3 n (-p) n) bl,
        Triangle (V3 n n p) (V3 p n n) (V3 p n p) (V3 n (-p) n) bl]
      left = [
        Triangle (V3 n n n) (V3 n p n) (V3 n p p) (V3 (-p) n n) bl,
        Triangle (V3 n p n) (V3 n p p) (V3 n n p) (V3 (-p) n n) bl]
      right = [
        Triangle (V3 p n n) (V3 p p n) (V3 p p p) (V3 p n n) bl,
        Triangle (V3 p p n) (V3 p p p) (V3 p n p) (V3 p n n) bl]
      front = [
        Triangle (V3 n n n) (V3 p n n) (V3 n p n) (V3 n n (-p)) bl,
        Triangle (V3 p n n) (V3 n p n) (V3 p p n) (V3 n n (-p)) bl]
      back = [
        Triangle (V3 n n p) (V3 p n p) (V3 n p p) (V3 n n p) bl,
        Triangle (V3 p n p) (V3 n p p) (V3 p p p) (V3 n n p) bl]

applyTransform :: Num a => M44 a -> Triangle a -> Triangle a
applyTransform mx (Triangle p1 p2 p3 normal c) = Triangle p1' p2' p3' normal' c  where
  p1' = view _xyz $ mx !* (point p1)
  p2' = view _xyz $ mx !* (point p2)
  p3' = view _xyz $ mx !* (point p3)
  normal' = view _xyz $ mx !* (vector normal) -- point?

data Camera a = Camera { _direction :: V3 a, _name :: String }
  deriving (Eq, Ord, Show, Functor)

makeLenses ''Camera

data SceneGraphF a b =
  EmptyGraph
  | Geo (Geometry a)
  | Group [b]
  | MatrixTransform (M44 a) b -- row-major 
  | Colour String b
  | Light
  | Cam (Camera a) 
  deriving (Eq, Ord, Show, Functor)

makePrisms ''SceneGraphF

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
  deriving (Eq, Ord, Show)

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

-- | Add a unit cube
cube :: SceneGraph Double
cube = SceneGraph $ Fix $ Geo Cube

-- | Assign a colour to objects
colour' :: SceneGraph a -> String -> SceneGraph a
colour' g = SceneGraph . Fix . flip Colour (_unSceneGraph g)

-- | Scale objects by a scalar
scale :: Num a => SceneGraph a -> a -> SceneGraph a
scale g = SceneGraph . Fix . flip MatrixTransform (_unSceneGraph g) . scaleM

-- | Translate objects by a vector
translate :: Num a => SceneGraph a -> V3 a -> SceneGraph a
translate g = SceneGraph . Fix . flip MatrixTransform (_unSceneGraph g) . translateM

-- | Empty scene graph is empty
empty :: SceneGraph a
empty = SceneGraph $ Fix EmptyGraph  

-- | Group a number of elements together
group :: [SceneGraph a] -> SceneGraph a
group = SceneGraph . Fix . Group . fmap _unSceneGraph

-- | Add a light
light :: SceneGraph a
light = SceneGraph $ Fix Light

-- | Add a named camera
camera :: V3 a -> String -> SceneGraph a
camera dir = SceneGraph . Fix . Cam . Camera dir

-- Matrix transforms
translateM :: Num a => V3 a -> M44 a
translateM t = identity & translation .~ t

scaleM :: Num a => a -> M44 a
scaleM factor = factor *!! identity

-- Evaluate scene graphs
triangles :: (Fractional a, Num a) => SceneGraph a -> [Triangle a]
triangles = cata t . _unSceneGraph where
  t g = case g of
    EmptyGraph -> []
    Geo g'     -> toTriangles g'
    Group ts   -> concat ts  
    MatrixTransform mx ts -> fmap (applyTransform mx) ts
    Colour cl ts -> fmap (set colour cl) ts
    Light -> []
    Cam _ -> [] 

simpleGraph = cube `colour'` "green" `translate` (V3 4 4 4)