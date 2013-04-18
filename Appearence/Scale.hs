module Appearence.Scale where

import Text.ParserCombinators.Parsec
import Data.Char
import Graphics.UI.GLUT
import Graphics.Rendering.OpenGL
import Data.List

-- | Obtains Scale Co-ordinates
getScaleVal :: String->(String,String)
getScaleVal [] = ([],[])
getScaleVal (x:xs) = do
                  if ((x == ',') || (x == '>'))
                    then ([],xs)
                    else
                     if (isSpace x)
                      then (fst(getScaleVal xs),snd(getScaleVal xs))
                      else (x:fst(getScaleVal xs),snd(getScaleVal xs))

-- | Calls HOpenGL scale function using obtained co-ordinates
scaleImage1 xs =let (r,rs)   = getScaleVal (tail xs)
                    (g, gs) = getScaleVal rs
                    (b,bs) = getScaleVal gs
                         in scale  (read r ::GLfloat) (read g ::GLfloat) (read b ::GLfloat)
                           

