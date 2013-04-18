module Shapes.Cone where

import Text.ParserCombinators.Parsec
import Data.Char
import Graphics.UI.GLUT
import Graphics.Rendering.OpenGL
import Shapes.Cylinder
import Shapes.Polygon
import Appearence.Translate
import Appearence.Color
import Appearence.Rotate
import Appearence.Scale
import Data.List

-- | Obtains cone parameters                          
coneFound::[String] -> IO ()
coneFound (x:r1:y:r2:rss) = let point1 = (returnArgsCy 1 [x])
                                rad    =  read (eatSpaces (r2))
                                point2 = (returnArgsCy 1 [y])
			     in  parseRCone rss (rad*3) (calcHeightCone (point1++point2))
					                   
-- | Calculates height of Cone
calcHeightCone [(x1,y1,z1),(x2,y2,z2)]=  (((x1-x2)*(x1-x2))+((y1-y2)*(y1-y2))+((z1-z2)*(z1-z2)))
			
			
-- | Obtains other attributes of cone such as Color, Translate etc.		
parseRCone [] r h= do currentColor $= Color4 1 0 0 0
                      hOpenGlCone r h
parseRCone (xs:xss) r h |(isInfixOf "color rgb" xs ) = do
                                                        let interm= snd(splitAt (head(elemIndices '<' xs)) xs)
                                                        do
                                                         setColorRGB (fst (splitAt (head(elemIndices '>' xs)) interm))
                                                         parseRCone xss r h
                        |(isInfixOf "translate" xs ) = do
                                                        let interm= snd(splitAt (head(elemIndices '<' xs)) xs)
                                                        do
                                                         translateImage (fst (splitAt (head(elemIndices '>' xs)) interm))
                                                         parseRCone xss r h
                        |(isInfixOf "rotate" xs )    = do
                                                        let interm= snd(splitAt (head(elemIndices '<' xs)) xs)
                                                        do
                                                         rotateImage (fst (splitAt (head(elemIndices '>' xs)) interm))
                                                         parseRCone xss r h
                        |(isInfixOf "scale" xs )    = do
                                                        let interm= snd(splitAt (head(elemIndices '<' xs)) xs)
                                                        do
                                                         scaleImage1 (fst (splitAt (head(elemIndices '>' xs)) interm))
                                                         parseRCone xss r h
                        |otherwise                  =  parseRCone xss r h
                        
-- | Renders HopneGL cone

hOpenGlCone r h= renderObject Solid (Cone r h 1000 1000)

