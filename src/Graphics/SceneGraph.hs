{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Graphics.SceneGraph where

import Control.Lens
import Control.Lens.Plated
import Data.Data
import Data.Data.Lens (uniplate)
import Data.Monoid
import Data.Typeable
import Linear.Matrix
import Linear.V3
import Linear.V4

data Triangle a = Triangle {
  _p1 :: V3 a,
  _p2 :: V3 a,
  _p3 :: V3 a,
  _normal :: V3 a,
  _color :: String
}
  deriving (Eq, Ord, Show, Functor, Data, Typeable)

makeLenses ''Triangle

data Geometry a = 
      Mesh [Triangle a]
    | Cube -- unit cube
    deriving (Eq, Ord, Show, Functor, Data, Typeable)

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
  deriving (Eq, Ord, Show, Functor, Data, Typeable)

makeLenses ''Camera

data SceneGraph a =
    EmptyGraph
    | Group [SceneGraph a]
    | Geo (Geometry a)
    | MatrixTransform (M44 a) (SceneGraph a)
    | Colour String (SceneGraph a)
    | Light
    | Cam (Camera a)
    deriving (Eq, Ord, Show, Functor, Data, Typeable)

instance Data a => Plated (SceneGraph a) where
  plate = uniplate

instance Monoid (SceneGraph a) where
    mempty = EmptyGraph
    mappend l r = case (l, r) of
      (EmptyGraph, _) -> r
      (_, EmptyGraph) -> l
      ((Group a), (Group b)) -> Group (a ++ b)
      ((Group a), r') -> Group (a ++ [r'])
      (l', (Group b)) -> Group (l':b)
      (l', r') -> Group [l', r']

makePrisms ''SceneGraph

-- | Constructors
cube :: SceneGraph Double
cube = Geo Cube

colour :: SceneGraph a -> String -> SceneGraph a
colour = flip Colour

scale :: Num a => SceneGraph a -> a -> SceneGraph a
scale g = flip MatrixTransform g . scaleM where
  scaleM factor = factor *!! identity

-- | Translate objects by a vector
translate :: Num a => SceneGraph a -> V3 a -> SceneGraph a
translate g = flip MatrixTransform g . translateM where
  translateM t = identity & translation .~ t

-- | Empty scene graph is empty
empty :: SceneGraph a
empty = EmptyGraph 

-- | Group a number of elements together
group :: [SceneGraph a] -> SceneGraph a
group = Group

-- | Add a light
light :: SceneGraph a
light = Light

-- | Add a named camera
camera :: V3 a -> String -> SceneGraph a
camera dir = Cam . Camera dir


-- | Rendering

-- | Optimise a scene graph by merging successive matrix transforms
optimise :: (Num a, Data a) => SceneGraph a -> SceneGraph a
optimise = transform $ \x -> case x of
  MatrixTransform m1 (MatrixTransform m2 a) -> MatrixTransform (m1 !*! m2) a
  _ -> x

-- Evaluate scene graphs
triangles :: (Data a, Fractional a, Num a) => SceneGraph a -> [Triangle a]
triangles = para tri where
  tri a l = case a of
    MatrixTransform mx _ -> fmap (applyTransform mx) $ concat l
    Colour cl _ -> fmap (set color cl) $ concat l
    Geo g'     -> toTriangles g'
    _ -> concat l 

render :: (Data a, Fractional a, Num a) => SceneGraph a -> [Triangle a] -- TODO: Should return primitives (camera, light, mesh) instead
render = triangles . optimise

simpleGraph = cube `colour` "green" `translate` (V3 4 4 4) `translate` (V3 1 1 1)