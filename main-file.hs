import Parser
import Polygon
<<<<<<< HEAD
--import Camera
import Circle
import Trans
=======
import Camera
>>>>>>> 6f9978a24100922641af1727dbef277a528f24af
import Text.ParserCombinators.Parsec
import Data.Char
import Graphics.UI.GLUT
import Graphics.Rendering.OpenGL
import Data.List

eatSpaces::String -> String
eatSpaces [] =[]
eatSpaces (x:xs) | (isSpace x) = eatSpaces xs
                 | (x== ',') = eatSpaces xs
                 |otherwise = x:(eatSpaces xs)

createAWindow windowName = do
           createWindow windowName
           displayCallback $= separateResult
<<<<<<< HEAD
           reshapeCallback $= Just Main.reshape
=======
           reshapeCallback $= Just reshape
>>>>>>> 6f9978a24100922641af1727dbef277a528f24af
           
separateResult = do
          clear [ColorBuffer,DepthBuffer]
          --currentColor $= Color4 1 0 0 0
          res <- mainComputation
         -- error (show res)
          case res of
            Right x -> displayFunction x
            Left x  -> error (show x)
          

reshape screenSize@(Size w h) = do
  viewport $= ((Position 0 0), screenSize)
  matrixMode $= Projection
  loadIdentity
  let near= 0.001
      far= 40
      fov= 90
      ang= (fov*pi)/(360)
      --ang = 45
      top= near / ( cos(ang) / sin(ang) )
      aspect = fromIntegral(w)/fromIntegral(h)
      right = top*aspect
  frustum (-right) right (-top) top near far
  matrixMode $= Modelview 0

displayFunction [] = flush
displayFunction (r:res) = do
                   --error (show res)
                   case r of 
                            ((x:y:xs)) ->   case x of
<<<<<<< HEAD
                                                  {-"polygon"  -> let myPoints = (returnArgsPoly (read (eatSpaces y)) xs)
                                                                 in
                                                                   parseRPolyArgs xs myPoints 
                                                  "triangle" -> let myPoints = (returnArgsPoly 3 (y:xs))
                                                                  in  hOpenGlPolygon myPoints-}

                                                 -- "camera"   -> parseCamera (y:xs)
                                                  "sphere"   ->let myPoints   = [(0.0,0.0,0.0)]--(returnArgsPoly 1 (y:xs))
                                                                   radius     = 0.5--(read (eatSpaces(head xs))::Float)
                                                                      in hOpenGlCircle radius myPoints 
=======
                                                  "polygon"  -> let myPoints = (returnArgsPoly (read (eatSpaces y)) xs)
                                                                 in
                                                                   parseRPolyArgs xs myPoints 
                                                  "triangle" -> let myPoints = (returnArgsPoly 3 (y:xs))
                                                                  in  hOpenGlPolygon myPoints

                                                  "camera"   -> parseCamera (y:xs)
>>>>>>> 6f9978a24100922641af1727dbef277a528f24af
                                                  rest       ->  print ([(0.0,0.0,0.0)])
                   flush
                   displayFunction res
                   
                   
                  

main = do
    (progName,_) <-getArgsAndInitialize
    createAWindow progName
    mainLoop

