{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Lens hiding (children)
import Data.Text (Text)
import Graphics.Geometry
import Graphics.Svg
import Linear.V3

import VirtualHom.Element
import VirtualHom.Html hiding (content, main)
import VirtualHom.Rendering(renderingOptions)
import VirtualHom.Bootstrap(container, row, btnDefault)
import VirtualHom.View(View, renderUI)

aCube :: Monad m => View m Int
aCube i = [svg
  & attributes . at "viewBox" ?~ "0 0 500 200" 
  & children .~ [
      g 
      & attributes . at "transform" ?~ "translate(50, 50)"
      & children .~ faces
  ]]
   where
      faces = fmap (toElm . fmap (polygon . fmap isometricProject)) cube
      cube = [a, b, c]
      u = 50
      a = ("red", [V3 0 0 0, V3 0 0 u, V3 u 0 u, V3 u 0 0])
      b = ("black", [V3 0 0 u, V3 u 0 u, V3 u u u, V3 0 u u])
      c = ("green", [V3 u 0 0, V3 u 0 u, V3 u u u, V3 u u 0])
      toElm (fll, e) = e & attributes . at "fill" ?~ fll

main :: IO ()
main = do
  let options = renderingOptions "iso-svg"
  let interp = return . runIdentity
  renderUI options aCube interp 1
