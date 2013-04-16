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
                         
coneFound::[String] -> IO ()
coneFound (x:r1:y:r2:rss) = let point1 = (returnArgsCy 1 [x])
                                rad    =  read (eatSpaces (r2))
                                point2 = (returnArgsCy 1 [y])
					                   in  parseRCone rss (rad*3) (calcHeightCone (point1++point2))
					                   

calcHeightCone [(x1,y1,z1),(x2,y2,z2)]=  (((x1-x2)*(x1-x2))+((y1-y2)*(y1-y2))+((z1-z2)*(z1-z2)))
					
parseRCone [] r h= do currentColor $= Color4 1 0 0 0
                      hOpenGlCone r h
parseRCone (xs:xss) r h = do
                             if (isInfixOf "color rgb" xs )
                              then
                               let interm= snd(splitAt (head(elemIndices '<' xs)) xs)
                                in
                                 do
                                  setColorRGB (fst (splitAt (head(elemIndices '>' xs)) interm))
                                  parseRCone xss r h
                              else
                               
                                       if (isInfixOf "translate" xs )
                                         then
                                           let interm= snd(splitAt (head(elemIndices '<' xs)) xs)
                                            in
                                             do
                                              translateImage (fst (splitAt (head(elemIndices '>' xs)) interm))
                                              parseRCone xss r h
                                         else
                                           if (isInfixOf "rotate" xs )
                                             then
                                               let interm= snd(splitAt (head(elemIndices '<' xs)) xs)
                                                in
                                                 do
                                                  rotateImage (fst (splitAt (head(elemIndices '>' xs)) interm))
                                                  parseRCone xss r h
                                             else
                                               if (isInfixOf "scale" xs )
                                               then
                                                 let interm= snd(splitAt (head(elemIndices '<' xs)) xs)
                                                  in
                                                   do
                                                    scaleImage1 (fst (splitAt (head(elemIndices '>' xs)) interm))
                                                    parseRCone xss r h
                                               else
                                                  parseRCone xss r h
                         --    hOpenGlCone r h

--3 6

hOpenGlCone r h= renderObject Solid (Cone r h 1000 1000)

