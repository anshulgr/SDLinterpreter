module Camera where
import Text.ParserCombinators.Parsec
import Data.Char
import Graphics.UI.GLUT
import Graphics.Rendering.OpenGL
import Data.List


--parse input
parseInput :: [String] -> [(String,GLdouble,GLdouble,GLdouble)]
parseInput [] = []
parseInput (xs:xss) =if ((isInfixOf "location" xs) || (isInfixOf "look_at" xs ) || (isInfixOf "direction" xs ) )
                         then
                            let (l1 , interm) = splitAt (head(elemIndices '<' xs)) xs
                               in
                                 let (x1,y1,z1) = getLocation (fst (splitAt (head(elemIndices '>' xs)) interm))
                                   in (l1 ,x1,y1,z1) :(parseInput xss)
                         else
                           if (isInfixOf "angle" xs)
                            then
                             let angle =  ( snd (splitAt (head (elemIndices ' ' (tail xs))) ( tail (xs) ) ) )
                               in ("angle",(read angle),0.0,0.0) :(parseInput xss) 
                             {- callFrustum angle
                                where angle = convertAngleType ( fst (splitAt (head (elemIndices ' ' (tail xs))) ( tail (xs) ) ) ) -}
                            else
                              parseInput xss

--call frustum
callFrustum ang = do
                     sz<-get screenSize
                     viewport $= ((Position 0 0), sz)
                     matrixMode $= Projection
                     loadIdentity
                     let near= 0.004
                         far= 20
                         fov= 50
                         (w,h) = getScreenSize sz
                         top= near -- / ( tan(ang) / cos(ang) )
                         aspect = fromIntegral(w)/fromIntegral(h)
                         right =  top*aspect
                     frustum (-right) (right*15) (-top) top near far
                     matrixMode $= Modelview 0



getScreenSize sz = 
                  case sz of
                    (Size w h) -> (w,h)
                 --   rest       -> error "not matching"









convertAngleType :: String -> GLdouble
convertAngleType xs = (read xs)
cameraFound xs = do
                  let location  = (findLoc (parseInput xs))
                      lookat    = findLookat (parseInput xs)
                      direction = findDirection (parseInput xs)
                    in setPointOfView location  lookat direction
                  callFrustum $ findAngle ( parseInput xs)
                  

findLoc res = let (a,x,y,z) = head ( filter (\(m,n,p,q) -> (isInfixOf "location" m ) ) res)
                in (Vertex3 x y z)

findLookat res = let (a,x,y,z) = head ( filter (\(m,n,p,q) -> (isInfixOf "look_at" m ) ) res)
                  in (Vertex3 x y z)

findDirection res = let (a,x,y,z) =head ( filter (\(m,n,p,q) -> (isInfixOf "direction" m ) ) res)
                     in (Vector3 x y z)

findAngle res = let (a,x,y,z) =head ( filter (\(m,n,p,q) -> (isInfixOf "angle" m ) ) res)
                     in x



getLocation :: String -> (GLdouble,GLdouble,GLdouble)
getLocation xs = let (r,rs)   = getVal (tail xs)
                    in 
                      let (g, gs) = getVal rs
                        in 
                          let (b,bs) = getVal gs
                            in ((read r),(read g),(read b))

getVal :: String -> (String,String)
getVal (x:xs) = do
                  if ((x == ',') || (x == '>'))
                    then ([],xs)
                    else
                     if (isSpace x)
                      then (fst(getVal xs),snd(getVal xs))
                      else (x:fst(getVal xs),snd(getVal xs))


setPointOfView location lookat direction = do 
                                             lookAt location lookat  direction
                                          -- lookAt (Vertex3 0 0 0) (Vertex3 0 0 0) (Vector3 0 0 0)
                                          --   clear [ColorBuffer,DepthBuffer]
                                             
