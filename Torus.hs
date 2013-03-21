module Torus where

import Text.ParserCombinators.Parsec
import Data.Char
import Graphics.UI.GLUT
import Graphics.Rendering.OpenGL


torusFound::[String] -> IO ()
torusFound (x:y:xs)= let rad1 = read x
			 rad2 = read y
		            in hOpenGlTorus rad1 rad2

hOpenGlTorus rad1 rad2 = renderObject Solid (Torus rad1 rad2 100 100)
