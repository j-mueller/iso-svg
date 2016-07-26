{-# LANGUAGE OverloadedStrings #-}
module Graphics.Svg where

import Control.Lens
import Data.Text (Text)
import VirtualHom.Internal.Element (Elem, attributes, elm, namespace)

xmlns :: Text
xmlns = "http://www.w3.org/2000/svg"

-- | Creat an element in the SVG namespace
svgElm :: Text -> Elem cb ()
svgElm t = elm t & namespace .~ xmlns

svg :: Elem cb ()
svg = svgElm "svg" 
  & attributes . at "version" ?~ "1.1"
  & attributes . at "xmlns" ?~ xmlns

circle :: Elem cb ()
circle = svgElm "circle"
