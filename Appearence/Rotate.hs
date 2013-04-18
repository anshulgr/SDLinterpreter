module Appearence.Rotate where

import Text.ParserCombinators.Parsec
import Data.Char
import Graphics.UI.GLUT
import Graphics.Rendering.OpenGL
import Data.List

-- | Obtains Rotate Co-ordinates
getRotateVal :: String->(String,String)
getRotateVal [] = ([],[])
getRotateVal (x:xs) = do
                  if ((x == ',') || (x == '>'))
                    then ([],xs)
                    else
                     if (isSpace x)
                      then (fst(getRotateVal xs),snd(getRotateVal xs))
                      else (x:fst(getRotateVal xs),snd(getRotateVal xs))

-- | Calls HOpenGL rotate function using obtained co-ordinates
rotateImage xs =let (r,rs)   = getRotateVal (tail xs)
                    in 
                      let (g, gs) = getRotateVal rs
                        in 
                          let (b,bs) = getRotateVal gs
                            in rotate (read r :: GLfloat) $ Vector3 1.0 ((read g ::GLfloat)/(read r :: GLfloat)) ((read b ::GLfloat)/(read r :: GLfloat))
                           

