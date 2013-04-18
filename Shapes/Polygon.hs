module Shapes.Polygon where

import Text.ParserCombinators.Parsec
import Data.Char
import Data.List
import Appearence.Color
import Appearence.Translate
import Appearence.Rotate
import Appearence.Scale
import Graphics.UI.GLUT
import Graphics.Rendering.OpenGL

-- | Removes spaces and commas from String
eatSpaces::String -> String
eatSpaces [] =[]
eatSpaces (x:xs) | (isSpace x) = eatSpaces xs
                 | (x== ',') = eatSpaces xs
                 |otherwise = x:(eatSpaces xs)

-- | Obtains Parameters of the polygon
polygonFound::[String] -> IO ()
polygonFound (x:xs)=let myPoints = (returnArgsPoly (read (eatSpaces x)) xs)
                                       in  parseRPolyArgs xs myPoints


-- | Creates list of tuples for vertices
returnArgsPoly::Int ->[String] -> [(GLfloat,GLfloat,GLfloat)]
returnArgsPoly 0 _ = []
returnArgsPoly a [] = []
returnArgsPoly a (x:xs) = (returnArgPoly x):(returnArgsPoly (a-1) xs)

-- | Separates x,y,z co-ordinates from the parameters 
myFunc::String -> (String,String)
myFunc [] =([],[])
myFunc (x:xs) | ((x == ',') || (x == '>')) = ([],xs)
              | (isSpace x)                = (fst(myFunc xs),snd(myFunc xs))
              | otherwise                  = (x:fst(myFunc xs),snd(myFunc xs))

-- | Creates tuple of co-ordinates for each vertex
returnArgPoly :: String -> (GLfloat,GLfloat,GLfloat)
returnArgPoly [] = (0.0,0.0,0.0)
returnArgPoly (x:xs) = if x== '<'
                         then let (y1,z1) = (myFunc xs)
                                  (y2,z2 )= (myFunc z1)
                                  (y3,z3 )= (myFunc z2)
                                         in ((read y1) , (read y2) , (read y3)) 
                         else returnArgPoly xs

-- | Obtains other attributes of the polygon
parseRPolyArgs [] myPoints= do currentColor $= Color4 1 0 0 0
                               hOpenGlPolygon myPoints
                               
parseRPolyArgs (xs:xss) mypoints        |(isInfixOf "color rgb" xs ) = do
                                                                        let interm= snd(splitAt (head(elemIndices '<' xs)) xs)
                                                                        do
                                                                         setColorRGB (fst (splitAt (head(elemIndices '>' xs)) interm))
                                                                         parseRPolyArgs xss mypoints
                                        |(isInfixOf "translate" xs ) = do
                                                                        let interm= snd(splitAt (head(elemIndices '<' xs)) xs)
                                                                        do
                                                                         translateImage (fst (splitAt (head(elemIndices '>' xs)) interm))
                                                                         parseRPolyArgs xss mypoints
                                        |(isInfixOf "rotate" xs )    = do
                                                                        let interm= snd(splitAt (head(elemIndices '<' xs)) xs)
                                                                        do
                                                                         rotateImage (fst (splitAt (head(elemIndices '>' xs)) interm))
                                                                         parseRPolyArgs xss mypoints
                                        |(isInfixOf "scale" xs )    = do
                                                                        let interm= snd(splitAt (head(elemIndices '<' xs)) xs)
                                                                        do
                                                                         scaleImage1 (fst (splitAt (head(elemIndices '>' xs)) interm))
                                                                         parseRPolyArgs xss mypoints
                                        |otherwise                  =  parseRPolyArgs xss mypoints
                        
 
-- | Renders HOpenGL polygon with the obtained parameters
hOpenGlPolygon args = renderPrimitive Polygon $mapM_ (\(x, y, z)->vertex $ Vertex3 x y z) args
                      

