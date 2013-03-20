module Camera where
import Text.ParserCombinators.Parsec
import Data.Char
import Graphics.UI.GLUT
import Graphics.Rendering.OpenGL
import Data.List

--parse 
parseInput :: [String] -> [(String,GLdouble,GLdouble,GLdouble)]
parseInput [] = []
parseInput (xs:xss) =if ((isInfixOf "location" xs) || (isInfixOf "look_at" xs ) || (isInfixOf "direction" xs ) )
                         then
                            let (l1 , interm) = splitAt (head(elemIndices '<' xs)) xs
                               in
                                 let (x1,y1,z1) = getLocation (fst (splitAt (head(elemIndices '>' xs)) interm))
                                   in (l1 ,x1,y1,z1) :(parseInput xss)
                         else
                           parseInput xss


parseCamera xs = setPointOfView location  lookat direction
                     where location  = (findLoc (parseInput xs))
                           lookat    = findLookat (parseInput xs)
                           direction = findDirection (parseInput xs)
                      

findLoc res = let (a,x,y,z) = head ( filter (\(m,n,p,q) -> (isInfixOf "location" m ) ) res)
                in (Vertex3 x y z)

findLookat res = let (a,x,y,z) = head ( filter (\(m,n,p,q) -> (isInfixOf "look_at" m ) ) res)
                  in (Vertex3 x y z)

findDirection res = let (a,x,y,z) =head ( filter (\(m,n,p,q) -> (isInfixOf "direction" m ) ) res)
                     in (Vector3 x y z)



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
                                             
