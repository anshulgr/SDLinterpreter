import Data.List
import Data.Char

--parse ploygon arguments
parseRPolyArgs xs = 
                        if (isInfixOf "color rgb" xs )
                         then
                          let interm= snd(splitAt (head(elemIndices '<' xs)) xs)
                            in
                             setColorRGB (fst (splitAt (head(elemIndices '>' xs)) interm))
                         else
                             (0,0,0)

getRGBVal :: String->(String,String)
getRGBVal [] = ([],[])
getRGBVal (x:xs) = do
                  if ((x == ',') || (x == '>'))
                    then ([],xs)
                    else
                     if (isSpace x)
                      then (fst(getRGBVal xs),snd(getRGBVal xs))
                      else (x:fst(getRGBVal xs),snd(getRGBVal xs))

--set color
setColorRGB :: String -> ( Int , Int , Int)
setColorRGB xs =  let (r,rs)   = getRGBVal (tail xs)
                    in 
                      let (g, gs) = getRGBVal rs
                        in 
                          let (b,bs) = getRGBVal gs
                            in ((read r),(read g),(read b))
 
