module Trans where

import Text.ParserCombinators.Parsec
import Data.Char
import Graphics.UI.GLUT
import Graphics.Rendering.OpenGL
import Data.List

myFunc::String -> (String,String)
myFunc [] =([],[])
myFunc (x:xs) = 
                if ((x == ',') || (x == '>'))
                 then ([],xs)
                 else
                  if (isSpace x)
                    then (fst(myFunc xs),snd(myFunc xs))
                    else (x:fst(myFunc xs),snd(myFunc xs))

returnArgTrans :: String -> (GLfloat,GLfloat,GLfloat)
returnArgTrans [] = (0.0,0.0,0.0)
returnArgTrans (x:xs) = if x== '<'
                         then let (y1,z1) = (myFunc xs)
                               in let (y2,z2 )= (myFunc z1)
                                    in let (y3,z3 )= (myFunc z2)
                                         in ((read y1) , (read y2) , (read y3)) 
                         else returnArgTrans xs


