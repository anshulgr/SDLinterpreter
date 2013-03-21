module Appearence.Light where
import Text.ParserCombinators.Parsec
import Data.Char
import Data.List
import Graphics.UI.GLUT
import Graphics.Rendering.OpenGL
import System.Environment
lightSourceFound (xs:xss) = do
                               let (x,y,z) = separateCoords xs
                                   (r,g,b) = separateCoords (head xss)
                                
                               lighting $= Enabled
                               position (Light 0) $= Vertex4 (read x) (read y) (read z) 1
                               --print "hi"
                               ambient (Light 0) $= Color4 (read r) (read g) (read b) 0
                               specular (Light 0) $= Color4 (read r) (read g) (read b) 0
                               diffuse (Light 0) $= Color4 (read r) (read g) (read b) 0
                               light (Light 0) $= Enabled 
                              -- clear [ColorBuffer,DepthBuffer]
                               
                               
                              
                      -- currentColor $= Color4 0 0 0 0
                       --clearColor $= Color4 0 0 0 1
                       
getRGBVal :: String->(String,String)
getRGBVal [] = ([],[])
getRGBVal (x:xs) = do
                    if ((x == ',') || (x == '>'))
                     then ([],xs)
                     else
                      if (isSpace x)
                       then (fst(getRGBVal xs),snd(getRGBVal xs))
                       else (x:fst(getRGBVal xs),snd(getRGBVal xs))

separateCoords:: String -> (String,String,String)
separateCoords xs = do    let interm  = snd(splitAt (head(elemIndices '<' xs)) xs)
                              interm1 = (fst (splitAt (head(elemIndices '>' xs)) interm))
                              (r,rs)  = getRGBVal (tail interm1)
                              (g,gs)  = getRGBVal rs
                              (b,bs)  = getRGBVal gs
                           in (r,g,b)
                        {-   
                         lighting $= Enabled
                         position (Light 0) $= Vertex4 1 0.4 0.8 1
                         ambient (Light 0) $= Color4 1 0 0 0
                         specular (Light 0) $= Color4 1 0 0 0
                         diffuse (Light 0) $= Color4 1 0 0 0
                         light (Light 0) $= Enabled
                         clear [ColorBuffer,DepthBuffer] 
                           
                                  setColorRGB (fst (splitAt (head(elemIndices '>' xs)) interm))
                                  -}
