module Shapes.Triangle where

import Text.ParserCombinators.Parsec
import Data.Char
import Graphics.UI.GLUT
import Graphics.Rendering.OpenGL
import Appearence.Color
import Appearence.Translate
import Appearence.Rotate
import Appearence.Scale
import Data.List

-- | Obtains parameters of the triangle
triangleFound::[String] -> IO ()
triangleFound (x:xs)= let myPoints =  (returnArgsTri 3 (x:xs))
                                          in  parseRTriangle xs myPoints

-- | Obtains other attributes of the triangle
parseRTriangle [] myPoints= do currentColor $= Color4 1 0 0 0
                               hOpenGlTriangle myPoints
parseRTriangle (xs:xss) mypoints       |(isInfixOf "color rgb" xs ) = do
                                                                        let interm= snd(splitAt (head(elemIndices '<' xs)) xs)
                                                                        do
                                                                         setColorRGB (fst (splitAt (head(elemIndices '>' xs)) interm))
                                                                         parseRTriangle xss mypoints
                                        |(isInfixOf "translate" xs ) = do
                                                                        let interm= snd(splitAt (head(elemIndices '<' xs)) xs)
                                                                        do
                                                                         translateImage (fst (splitAt (head(elemIndices '>' xs)) interm))
                                                                         parseRTriangle xss mypoints
                                        |(isInfixOf "rotate" xs )    = do
                                                                        let interm= snd(splitAt (head(elemIndices '<' xs)) xs)
                                                                        do
                                                                         rotateImage (fst (splitAt (head(elemIndices '>' xs)) interm))
                                                                         parseRTriangle xss mypoints
                                        |(isInfixOf "scale" xs )    = do
                                                                        let interm= snd(splitAt (head(elemIndices '<' xs)) xs)
                                                                        do
                                                                         scaleImage1 (fst (splitAt (head(elemIndices '>' xs)) interm))
                                                                         parseRTriangle xss mypoints
                                        |otherwise                  =  parseRTriangle xss mypoints

-- | Creates list of tuples for vertices
returnArgsTri::Int ->[String] -> [(GLfloat,GLfloat,GLfloat)]
returnArgsTri 0 _ = []
returnArgsTri a [] = []
returnArgsTri a (x:xs) = (returnArgTri x):(returnArgsTri (a-1) xs)

-- | Separates x,y,z co-ordinates from the parameters 
myFunc1::String -> (String,String)
myFunc1 [] =([],[])
myFunc1 (x:xs) | ((x == ',') || (x == '>')) = ([],xs)
               | (isSpace x)                = (fst(myFunc1 xs),snd(myFunc1 xs))
               | otherwise                  = (x:fst(myFunc1 xs),snd(myFunc1 xs))
                
-- | Creates tuple of co-ordinates for each vertex
returnArgTri :: String -> (GLfloat,GLfloat,GLfloat)
returnArgTri [] = (0.0,0.0,0.0)
returnArgTri (x:xs) |( x== '<') = let (y1,z1) = (myFunc1 xs)
                                   (y2,z2 ) = (myFunc1 z1)
                                   (y3,z3 )= (myFunc1 z2)
                                     in ((read y1) , (read y2) , (read y3)) 
                    |otherwise  = returnArgTri xs

-- | Renders HOpenGL triangle with obtained parameters
hOpenGlTriangle args= renderPrimitive Triangles $mapM_ (\(x, y, z)->vertex $ Vertex3 x y z) args

