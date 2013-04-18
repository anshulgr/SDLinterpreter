module Appearence.Translate where

import Text.ParserCombinators.Parsec
import Data.Char
import Graphics.UI.GLUT
import Graphics.Rendering.OpenGL
import Data.List

-- | Obtains Translate Co-ordinates
getTranslateVal :: String->(String,String)
getTranslateVal [] = ([],[])
getTranslateVal (x:xs) = do
                  if ((x == ',') || (x == '>'))
                    then ([],xs)
                    else
                     if (isSpace x)
                      then (fst(getTranslateVal xs),snd(getTranslateVal xs))
                      else (x:fst(getTranslateVal xs),snd(getTranslateVal xs))

-- | Calls HOpenGL translate function using obtained co-ordinates
translateImage xs =let (r,rs)   = getTranslateVal (tail xs)
                    in 
                      let (g, gs) = getTranslateVal rs
                        in 
                          let (b,bs) = getTranslateVal gs
                            in translate $ Vector3 (read r ::GLfloat) (read g ::GLfloat) (read b ::GLfloat)
                           

