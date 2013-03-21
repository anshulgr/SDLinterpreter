module Polygon where

import Text.ParserCombinators.Parsec
import Data.Char
import Graphics.UI.GLUT
import Graphics.Rendering.OpenGL
import Data.List
returnArgsPoly::Int ->[String] -> [(GLfloat,GLfloat,GLfloat)]
returnArgsPoly 0 _ = []
returnArgsPoly a [] = []
returnArgsPoly a (x:xs) = (returnArgPoly x):(returnArgsPoly (a-1) xs)

myFunc::String -> (String,String)
myFunc [] =([],[])
myFunc (x:xs) = 
                if ((x == ',') || (x == '>'))
                 then ([],xs)
                 else
                  if (isSpace x)
                    then (fst(myFunc xs),snd(myFunc xs))
                    else (x:fst(myFunc xs),snd(myFunc xs))

returnArgPoly :: String -> (GLfloat,GLfloat,GLfloat)
returnArgPoly [] = (0.0,0.0,0.0)
returnArgPoly (x:xs) = if x== '<'
                         then let (y1,z1) = (myFunc xs)
                               in let (y2,z2 )= (myFunc z1)
                                    in let (y3,z3 )= (myFunc z2)
                                         in ((read y1) , (read y2) , (read y3)) 
                         else returnArgPoly xs

parseRPolyArgs [] myPoints= do currentColor $= Color4 1 0 0 0
                               translate $ (Vector3 0.2 0.2 (0.2::GLfloat))
                               hOpenGlPolygon myPoints
parseRPolyArgs (xs:xss) myPoints = do
                             if (isInfixOf "color rgb" xs )
                              then
                               let interm= snd(splitAt (head(elemIndices '<' xs)) xs)
                                in
                                  setColorRGB (fst (splitAt (head(elemIndices '>' xs)) interm))
                              else
                                 parseRPolyArgs xss myPoints
                          --   translate $ (Vector3 0.2 0.2 (0.2::GLfloat))
                             hOpenGlPolygon myPoints

getRGBVal :: String->(String,String)
getRGBVal [] = ([],[])
getRGBVal (x:xs) = do
                  if ((x == ',') || (x == '>'))
                    then ([],xs)
                    else
                     if (isSpace x)
                      then (fst(getRGBVal xs),snd(getRGBVal xs))
                      else (x:fst(getRGBVal xs),snd(getRGBVal xs))

--setColorRGB :: String -> ( Int , Int , Int)
setColorRGB xs =  let (r,rs)   = getRGBVal (tail xs)
                    in 
                      let (g, gs) = getRGBVal rs
                        in 
                          let (b,bs) = getRGBVal gs
                            in currentColor $= Color4 (read r) (read g) (read b) 0
 

hOpenGlPolygon args =  --loadIdentity
                          --translate $ (Vector3 0.2 0.2 (0.2::GLfloat))
                          renderPrimitive Polygon $mapM_ (\(x, y, z)->vertex $ Vertex3 x y z) args
                          

