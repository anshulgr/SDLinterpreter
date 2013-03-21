module Cube where
import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT as GLUT


makeVertexes :: [(GLfloat,GLfloat,GLfloat)] -> IO ()
makeVertexes = mapM_ (\(x,y,z)->vertex $ Vertex3 x y z)

renderAs :: PrimitiveMode -> [(GLfloat,GLfloat,GLfloat)] -> IO ()
renderAs figure ps = renderPrimitive figure $ makeVertexes ps


cube l = renderAs Quads corners
  where
  corners =
   [(l,0,l),(0,0,l),(0,l,l),(l,l,l)
   ,(l,l,l),(l,l,0),(l,0,0),(l,0,l)
   ,(0,0,0),(l,0,0),(l,0,l),(0,0,l)
   ,(l,l,0),(0,l,0),(0,0,0),(l,0,0)
   ,(0,l,l),(l,l,l),(l,l,0),(0,l,0)
   ,(0,l,l),(0,l,0),(0,0,0),(0,0,l)
   ]


main = do
   (progName,_) <- getArgsAndInitialize
   createWindow progName
   displayCallback $= display
   mainLoop



display = do
   clear [ColorBuffer]
   rotate 40 (Vector3 1 1 (1::GLfloat))
   cube 0.5
   loadIdentity
   flush











renderInWindow displayFunction = do
       (progName,_) <- getArgsAndInitialize
       createWindow progName
       displayCallback $= displayFunction
       mainLoop
    
