module Parser where

import Text.ParserCombinators.Parsec
import Data.Char
import Graphics.UI.GLUT
import Graphics.Rendering.OpenGL
<<<<<<< HEAD
import Data.List
import Data.String
import System.Environment
import System.Directory
--import Filesystem.Path.CurrentOS
=======
>>>>>>> f339ca7374459d61eee5693947dee4880cede7c3



csvFile :: GenParser Char st [[String]]
csvFile = 
    do result <- many line
       eof
       return result

-- Each line contains 1 or more cells, separated by a bracket '}' or newline '\n'
line :: GenParser Char st [String]
line = 
    do result <- cells 0
       eol                       -- end of line
       return result
       
-- Build up a list of cells.  Try to parse the first cell, then figure out 
-- what ends the cell.
cells ::(Num a,Eq a) => a-> GenParser Char st [String]
cells c = 
    do first <- cellContent 
       next <- remainingCells c
       if (foldr (\a b -> (isSpace a) && b) True first )
       then
          return (next)
       else
          return ((removeSpaces  first) : next)

-- The cell either ends with a comma, indicating that 1 or more cells follow,
-- or it doesn't, indicating that we're at the end of the cells for this line
remainingCells ::(Num a,Eq a) => a-> GenParser Char st [String]
remainingCells c = 
                  try (char '\n' >> cells c) -- Found comma? More cells coming
                  <|>try (char '{'  >> (cells (c+1)) ) 
          --        <|>try (char '{' >> cells)
                  <|>(char '}' >> if c == 1 
                                   then return [] 
                                   else (cells (c-1)))
-- <|>(char '}' >> return [])
                  <|> (return [])
-- Each cell contains 0 or more characters, which must not be a { or \n or
-- EOL
cellContent :: GenParser Char st String
cellContent = 
    many (noneOf "{\n}")
       

-- The end of line character is \n
eol :: GenParser Char st Char
eol = char '\n' <|> char '}'

parseCSV :: String -> Either ParseError [[String]]
parseCSV input = parse csvFile "(unknown)" input

removeSpaces [] = []
removeSpaces (x:[]) | isSpace x =[]
                    | otherwise = [x]
removeSpaces (x:y:xs) | (isSpace x == True && isSpace y == False)  = x:y:(removeSpaces xs)
                      | (isSpace x == True && isSpace y == True)  = (removeSpaces (y:xs))
                      | otherwise = x : (removeSpaces (y:xs))
<<<<<<< HEAD
mainComputation= do
         contents <- readFile "povray-polygon.pov"
=======
mainComputation arg  = do
         contents <- readFile arg--"cylinder.pov"
>>>>>>> f339ca7374459d61eee5693947dee4880cede7c3
         return $ parseCSV contents



<<<<<<< HEAD

--remove comments
comment :: GenParser Char st ()
comment =
    try(string "//" >> manyTill anyChar newline >> spaces >> return ()) <|>
    try(string "/*" >> manyTill anyChar (string "*/") >>  spaces >> return ())<|>
    try(string "#" >> manyTill anyChar newline >> spaces >> return ())
  

notComment = manyTill anyChar (lookAhead (comment <|> eof))

eatComments :: GenParser Char st String
eatComments = do
  optional comment
  xs <- sepBy notComment comment
  optional comment
  return $ intercalate "" xs





parseCSV1 :: String -> Either ParseError String
parseCSV1 input = parse eatComments "(unknown)" input





{-mainComputation1= do
                  contents <- readFile "povray-polygon.pov"
                  return $parseCSV1 contents


-}




mainC=do
      file<-getArgs
      contents<-readFile (head file)
      --contents<-readFile "povray-polygon.pov"
      --contents<-getContents
      let after=parseCSV1 contents
          in case after of
                  Right v->return $ parseCSV v




--type FilePath=String

getFileName::String->FilePath
getFileName s= read s
























=======
>>>>>>> f339ca7374459d61eee5693947dee4880cede7c3
