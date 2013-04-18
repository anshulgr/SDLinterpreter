module Main(main) where
import Parser
import Shapes.Polygon
import Shapes.Triangle
import Shapes.Torus
import Shapes.Cylinder
import Shapes.Cone
import Shapes.Sphere
import Appearence.Camera
import Appearence.Light
import Text.ParserCombinators.Parsec
import Data.Char
import Graphics.UI.GLUT
import Graphics.Rendering.OpenGL
import System.Environment

-- | Gets input file as command line argument and initializes the arguments of HOpenGL
-- | Main
main:: IO()
main = do
    (arg1:args) <- getArgs
    (progName,_) <-getArgsAndInitialize
    createAWindow progName arg1
    mainLoop


-- | Creates a new HOpenGL window
createAWindow windowName arg = do
           createWindow windowName
           displayCallback $= separateResult arg


-- | Collects tokens of POV-Ray file from Parser
separateResult arg = do
          clear [ColorBuffer,DepthBuffer]
          res <- mainComputation arg
          case res of
            Right x -> displayFunction x
            Left x  -> error (show x)

-- | Recognizes keywords and calls corresponding function
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
          displayFunction xs

 
defaultFunc = print ("Rendering ...")


