module Shapes.Sphere where

import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT as GLUT
import Shapes.Polygon
import Text.ParserCombinators.Parsec
import Data.Char
import Graphics.UI.GLUT
import Graphics.Rendering.OpenGL
import Shapes.Polygon
import Shapes.Cylinder
import Appearence.Color
import Appearence.Translate
import Appearence.Rotate
import Appearence.Scale
import Data.List
import Appearence.Camera

-- | Creates HOpenGL vertices
makeVertexes :: [(GLfloat,GLfloat,GLfloat)] -> IO ()
makeVertexes = mapM_ (\(x,y,z)->vertex $ Vertex3 x y z)

-- | Renders HOpenGL Sphere from obtained parameters
renderAs :: PrimitiveMode -> [(GLfloat,GLfloat,GLfloat)] -> IO ()
renderAs figure ps = renderPrimitive figure $ makeVertexes ps

-- | Displays created vertices
displayPoints points primitiveShape = do  
                        renderAs primitiveShape points
                        flush

-- | Creates Circle object
circlePoints radius x y z number=do 
                                    [let alpha = twoPi * i /number
                                     in (radius*(sin (alpha)) ,radius * (cos (alpha)),0)|i <-[1,2..number]]
       where
         twoPi = 2*pi


circle radius x y z = circlePoints radius x y z 10000

renderCircle r x y z = displayPoints (circle r x y z) LineLoop
fillCircle r x y z= do
                     displayPoints (circle r x y z) Polygon
                  
-- | Renders HOpenGL solid sphere
hOpenGlCircle myPoints radius = renderObject Solid (Sphere' radius 1000 1000)
getCord n (x,y,z)
		| n==1 = x	
		| n==2 = y
		| n==3 = z

-- | Obtains parameters of the sphere
sphereFound::[String] -> IO ()
sphereFound (x:xs) = let myPoints = (returnArgsCy 1 (x:xs))
                         rad    =  read (eatSpaces (head (xs)))
				in  parseRSp xs myPoints rad 

parseRSp []  myPoints r  = do 
                             currentColor $= Color4 1 0 0 0
                             hOpenGlCircle  myPoints r 
                           
parseRSp (xs:xss) myPoints r           |(isInfixOf "color rgb" xs ) = do
                                                                        let interm= snd(splitAt (head(elemIndices '<' xs)) xs)
                                                                        do
                                                                         setColorRGB (fst (splitAt (head(elemIndices '>' xs)) interm))
                                                                         parseRSp xss myPoints r 
                                        |(isInfixOf "translate" xs ) = do
                                                                        let interm= snd(splitAt (head(elemIndices '<' xs)) xs)
                                                                        do
                                                                         translateImage (fst (splitAt (head(elemIndices '>' xs)) interm))
                                                                         parseRSp xss myPoints r 
                                        |(isInfixOf "rotate" xs )    = do
                                                                        let interm= snd(splitAt (head(elemIndices '<' xs)) xs)
                                                                        do
                                                                         rotateImage (fst (splitAt (head(elemIndices '>' xs)) interm))
                                                                         parseRSp xss myPoints r 
                                        |(isInfixOf "scale" xs )    = do
                                                                        let interm= snd(splitAt (head(elemIndices '<' xs)) xs)
                                                                        do
                                                                         scaleImage1 (fst (splitAt (head(elemIndices '>' xs)) interm))
                                                                         parseRSp xss myPoints r 
                                        |otherwise                  =  parseRSp xss myPoints r 
                                 

                                
