module Appearence.Color where

import Text.ParserCombinators.Parsec
import Data.Char
import Graphics.UI.GLUT
import Graphics.Rendering.OpenGL
import Data.List

-- | Separates r,g,b parameters
getRGBVal :: String->(String,String)
getRGBVal [] = ([],[])
getRGBVal (x:xs) = do
                  if ((x == ',') || (x == '>'))
                    then ([],xs)
                    else
                     if (isSpace x)
                      then (fst(getRGBVal xs),snd(getRGBVal xs))
                      else (x:fst(getRGBVal xs),snd(getRGBVal xs))

-- | Sets current color using obtained r,g,b values
setColorRGB xs =  let (r,rs)   = getRGBVal (tail xs)
                    in 
                      let (g, gs) = getRGBVal rs
                        in 
                          let (b,bs) = getRGBVal gs
                            in do --clearColor $= Color4 0 0 0 0
                                 -- clear [ColorBuffer,DepthBuffer]
                                  currentColor $= Color4 (read r) (read g) (read b) 0
                               --   crMat (read r) (read g) (read b)



crMat rd gd bd = do
                  materialDiffuse Front $= Color4 rd gd bd 0.0
                  --materialAmbient Front $= Color4 rd gd bd 0.0
                  --materialSpecular Front $= Color4 rd gd bd 0.0
                  materialShininess Front $= 0
                  materialDiffuse Back $= Color4 rd gd bd 0.0
                  materialSpecular Back $= Color4 rd gd bd 0.0
                  materialShininess Back $= 0



