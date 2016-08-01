{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
module Graphics.SceneGraph where

import Control.Applicative
import Control.Lens
import Control.Lens.Plated
import Data.Data
import Data.Data.Lens (uniplate)
import Data.Monoid
import Data.Typeable
import Linear.Matrix
import Linear.V3
import Linear.V4

data Triangle v a = Triangle {
  _p1 :: v a,
  _p2 :: v a,
  _p3 :: v a,
  _normal :: v a,
  _color :: String
}
  deriving (Eq, Ord, Show, Functor, Data, Typeable)

makeLenses ''Triangle

normaliseTriangle :: Fractional a => Triangle V4 a -> Triangle V3 a
normaliseTriangle = Triangle <$> 
  view (p1.to normalizePoint) <*>
  view (p2.to normalizePoint) <*> 
  view (p3.to normalizePoint) <*> 
  view (normal._xyz) <*>
  view color 

newtype Mesh v a = Mesh { _unMesh :: [Triangle v a] }
  deriving (Eq, Ord, Show, Functor, Data, Typeable)

makeLenses ''Mesh

data Geometry v a = 
      Msh (Mesh v a)
    | Cube -- unit cube
    deriving (Eq, Ord, Show, Functor, Data, Typeable)

makePrisms ''Geometry

toTriangles :: (Fractional a, Num a) => Geometry V4 a -> Mesh V4 a
toTriangles g = case g of
  Msh mesh -> mesh
  Cube -> Mesh $ top <> bottom <> left <> right <> front <> back where
      bl = "black"
      n = (- 0.5)
      p = 0.5
      top = [
        Triangle (V4 n p n 1) (V4 n p p 1) (V4 p p n 1) (V4 n p n 1) bl, 
        Triangle (V4 n p p 1) (V4 p p n 1) (V4 p p p 1) (V4 n p n 1) bl]
      bottom = [
        Triangle (V4 n n n 1) (V4 n n p 1) (V4 p n n 1) (V4 n (-p) n 1) bl,
        Triangle (V4 n n p 1) (V4 p n n 1) (V4 p n p 1) (V4 n (-p) n 1) bl]
      left = [
        Triangle (V4 n n n 1) (V4 n p n 1) (V4 n p p 1) (V4 (-p) n n 1) bl,
        Triangle (V4 n p n 1) (V4 n p p 1) (V4 n n p 1) (V4 (-p) n n 1) bl]
      right = [
        Triangle (V4 p n n 1) (V4 p p n 1) (V4 p p p 1) (V4 p n n 1) bl,
        Triangle (V4 p p n 1) (V4 p p p 1) (V4 p n p 1) (V4 p n n 1) bl]
      front = [
        Triangle (V4 n n n 1) (V4 p n n 1) (V4 n p n 1) (V4 n n (-p) 1) bl,
        Triangle (V4 p n n 1) (V4 n p n 1) (V4 p p n 1) (V4 n n (-p) 1) bl]
      back = [
        Triangle (V4 n n p 1) (V4 p n p 1) (V4 n p p 1) (V4 n n p 1) bl,
        Triangle (V4 p n p 1) (V4 n p p 1) (V4 p p p 1) (V4 n n p 1) bl]

transformMesh :: Num a => M44 a -> Triangle V4 a -> Triangle V4 a
transformMesh mx (Triangle p1 p2 p3 normal c) = Triangle p1' p2' p3' normal' c  where
  p1' = mx !* p1
  p2' = mx !* p2
  p3' = mx !* p3
  normal' = mx !* normal

normaliseMesh :: Fractional a => Mesh V4 a -> Mesh V3 a
normaliseMesh = over (unMesh.mapped) normaliseTriangle

data Camera v a = Camera { 
  _cameraPos :: v a,
  _direction :: v a,
  _name :: String }
  deriving (Eq, Ord, Show, Functor, Data, Typeable)

makeLenses ''Camera

transformCam :: Num a => M44 a -> Camera V4 a -> Camera V4 a
transformCam mx = over cameraPos tfn . over direction tfn where
  tfn = (!*) mx

normaliseCamera :: Fractional a => Camera V4 a -> Camera V3 a
normaliseCamera = Camera <$> 
      view (cameraPos.to normalizePoint) <*>
      view (direction._xyz) <*>
      view name

data Light v a = Light { 
  _lightPos :: v a,
  _strength :: Double
  } 
  deriving (Eq, Ord, Show, Functor, Data, Typeable)

makeLenses ''Light

transformLight :: Num a => M44 a -> Light V4 a -> Light V4 a
transformLight mx = over lightPos (mx !*)

normaliseLight :: Fractional a => Light V4 a -> Light V3 a
normaliseLight = Light <$> view (lightPos.to normalizePoint) <*> view strength

data SceneGraph v a =
    EmptyGraph
    | Group [SceneGraph v a]
    | Geo (Geometry v a)
    | MatrixTransform (M44 a) (SceneGraph v a)
    | Colour String (SceneGraph v a)
    | Lght (Light v a)
    | Cam (Camera v a)
    deriving (Eq, Ord, Show, Functor, Data, Typeable)

instance (Data a, Data (v a), Typeable v) => Plated (SceneGraph v a) where
  plate = uniplate

instance Monoid (SceneGraph v a) where
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

-- | Add a unit cube at the origin
cube :: SceneGraph V4 Double
cube = Geo Cube

-- | Set the colour of the objects of a scene graph
colour :: SceneGraph v a -> String -> SceneGraph v a
colour = flip Colour

-- | Scale all objects in a scene graph by a factor
scale :: Num a => SceneGraph V4 a -> a -> SceneGraph V4 a
scale g = flip MatrixTransform g . scaleM where
  scaleM factor = factor *!! identity

-- | Translate all objects by a vector
translate :: Num a => SceneGraph V4 a -> V3 a -> SceneGraph V4 a
translate g = flip MatrixTransform g . translateM where
  translateM t = identity & translation .~ t

-- | Empty scene graph is empty
empty :: SceneGraph v a
empty = EmptyGraph 

-- | Group a number of elements together
group :: [SceneGraph v a] -> SceneGraph v a
group = Group

-- | Add a light
light :: Num a => SceneGraph V4 a
light = Lght $ Light 0 1

-- | Add a named camera at the origin
camera' :: Num a => V4 a -> String -> SceneGraph V4 a
camera' dir = Cam . Camera 0 dir

-- | Rendering

-- | Optimise a scene graph by merging successive matrix transforms
optimise :: (Num a, Data a) => SceneGraph V4 a -> SceneGraph V4 a
optimise = transform $ \x -> case x of
  MatrixTransform m1 (MatrixTransform m2 a) -> MatrixTransform (m1 !*! m2) a
  _ -> x

data Primitive v a =
    PrimLight (Light v a)
    | PrimCam (Camera v a)
    | PrimMesh (Mesh v a)
  deriving (Eq, Ord, Show, Functor, Data, Typeable)

makePrisms ''Primitive

normalise :: (Fractional a) => Primitive V4 a -> Primitive V3 a
normalise prm = case prm of
  PrimLight l -> PrimLight $ normaliseLight l
  PrimCam c -> PrimCam $ normaliseCamera c    
  PrimMesh msh -> PrimMesh $ normaliseMesh msh

-- Evaluate scene graphs.
-- Applies all matrix transformations and colour settings
evaluate :: (Data a, Fractional a, Num a) => SceneGraph V4 a -> [Primitive V4 a]
evaluate = para tri where
  tri a l = case a of
    Geo g'     -> [PrimMesh $ toTriangles g']
    Lght lg    -> [PrimLight lg]
    Cam c      -> [PrimCam c]
    MatrixTransform mx _ ->
      fmap (pm . pl . pc) $ concat l where
        pm = over (_PrimMesh . unMesh . mapped) $ transformMesh mx
        pl = over _PrimLight $ transformLight mx 
        pc = over _PrimCam $ transformCam mx
    Colour cl _ ->
      fmap (over (_PrimMesh . unMesh . mapped) setCl) $ concat l where
        setCl = set color cl
    _ -> concat l

-- | Apply all transformations in a scene graph and project the results
-- to three-dimensional space.
render :: (Data a, Fractional a, Num a) => SceneGraph V4 a -> [Primitive V3 a]
render = fmap normalise . evaluate . optimise

simpleGraph = cube `colour` "green" `translate` (V3 4 4 4) `translate` (V3 1 1 1)