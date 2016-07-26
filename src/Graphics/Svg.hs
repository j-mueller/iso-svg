{-# LANGUAGE OverloadedStrings #-}
module Graphics.Svg where

import Control.Lens
import Data.List (intercalate)
import Data.Maybe (listToMaybe)
import Data.Monoid
import qualified Data.Text as T
import Data.Text (Text)
import Linear.V2
import VirtualHom.Internal.Element (Elem, attributes, elm, namespace)

xmlns :: Text
xmlns = "http://www.w3.org/2000/svg"

-- | Create an element in the SVG namespace
svgElm :: Text -> Elem cb ()
svgElm t = elm t & namespace .~ xmlns

svg :: Elem cb ()
svg = svgElm "svg" 
  & attributes . at "version" ?~ "1.1"
  & attributes . at "xmlns" ?~ xmlns

circle :: Elem cb ()
circle = svgElm "circle"

path :: Elem cb ()
path = svgElm "path"

-- | Create a closed path from a list of points
polygon :: Show a => [V2 a] -> Elem cb ()
polygon pts = path & attributes . at "d" .~ thePath where
  thePath = fmap makePoints $ listToMaybe pts
  makePoints firstPoint = moveTo firstPoint <> foldMap lineTo (drop 1 pts) <> "z"
  moveTo (V2 x y) = "M" <> (T.pack $ show x) <> "," <> (T.pack $ show y)
  lineTo (V2 x y) = "L" <> (T.pack $ show x) <> "," <> (T.pack $ show y)
    