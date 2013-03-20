import Parser
<<<<<<< HEAD
import Polygon
import Camera
=======
import Shapes.Polygon
import Shapes.Triangle
import Shapes.Torus
import Shapes.Cylinder
import Shapes.Cone
import Shapes.Sphere
import Appearence.Camera
import Appearence.Light
{-import Parser
import Appearence
import Shapes-}
>>>>>>> f339ca7374459d61eee5693947dee4880cede7c3
import Text.ParserCombinators.Parsec
import Data.Char
import Graphics.UI.GLUT
import Graphics.Rendering.OpenGL
<<<<<<< HEAD
import Data.List

eatSpaces::String -> String
eatSpaces [] =[]
eatSpaces (x:xs) | (isSpace x) = eatSpaces xs
                 | (x== ',') = eatSpaces xs
                 |otherwise = x:(eatSpaces xs)

createAWindow windowName = do
           createWindow windowName
           displayCallback $= separateResult
           reshapeCallback $= Just reshape
           
separateResult = do
          clear [ColorBuffer,DepthBuffer]
          --currentColor $= Color4 1 0 0 0
          res <- mainC
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
                                                  "polygon"  -> let myPoints = (returnArgsPoly (read (eatSpaces y)) xs)
                                                                 in
                                                                   parseRPolyArgs xs myPoints 
                                                  "triangle" -> let myPoints = (returnArgsPoly 3 (y:xs))
                                                                  in  hOpenGlPolygon myPoints

                                                  "camera"   -> parseCamera (y:xs)
                                                  rest       ->  print ([(0.0,0.0,0.0)])
                   flush
                   displayFunction res
                   
                   
                  

main = do
    (progName,_) <-getArgsAndInitialize
    createAWindow progName
=======
import System.Environment

{-hello from ladha-}
createAWindow windowName arg = do
           --depthFunc $= Just Less
           createWindow windowName
           
          
        --   diffuse (Light 0) $= Color4 0 1 0 1
        --   specular (Light 0) $= Color4 0 0 1 1
        
           displayCallback $= separateResult arg


separateResult arg = do
          clear [ColorBuffer,DepthBuffer]
          res <- mainComputation arg
          case res of
            Right x -> displayFunction x
            Left x  -> error (show x)


displayFunction [] = flush
displayFunction (x:xs) = do
         
          case (head x) of
                             "polygon"      -> polygonFound (tail x)
                             "triangle"     -> triangleFound (tail x)
                             "cylinder"     -> cylinderFound (tail x)
                             "cone"         -> coneFound (tail x)
                             "torus"        -> torusFound (tail x)
                             "sphere"       -> sphereFound (tail x)
                             "camera"       -> cameraFound (tail x)
                             "light_source" -> lightSourceFound (tail x)
                             
                             res        -> defaultFunc  
         -- flush
          displayFunction xs
       {--    displayFunction xs--}



{--change testing comment -}


defaultFunc = print ([(0.0,0.0,0.0)])
main = do
    (arg1:args) <- getArgs
    (progName,_) <-getArgsAndInitialize
    createAWindow progName arg1
>>>>>>> f339ca7374459d61eee5693947dee4880cede7c3
    mainLoop

