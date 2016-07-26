{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Lens hiding (children)
import Graphics.Svg
import Linear.V2

import VirtualHom.Element
import VirtualHom.Html hiding (content, main)
import VirtualHom.Rendering(renderingOptions)
import VirtualHom.Bootstrap(container, row, btnDefault)
import VirtualHom.View(View, renderUI)

aCircle :: Monad m => View m Int
aCircle i = [svg
  & attributes . at "viewBox" ?~ "0 0 500 200" 
  & children .~ [
    circle 
      & attributes . at "cx" ?~ "60"
      & attributes . at "cy" ?~ "60"
      & attributes . at "r" ?~ "50",
    polygon [V2 10 10, V2 20 20, V2 30 20]
    ]]

main :: IO ()
main = do
  let options = renderingOptions "iso-svg"
  let interp = return . runIdentity
  renderUI options aCircle interp 1

