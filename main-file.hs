import Parser
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
import Text.ParserCombinators.Parsec
import Data.Char
import Graphics.UI.GLUT
import Graphics.Rendering.OpenGL
import System.Environment


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
    mainLoop

