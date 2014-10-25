import Data.List.Split
import Text.Read
import qualified Data.Text as T

--get the code
badCode = readFile ("./se2s03/BadCode.java")

parseFileText = do
					badCodeInString <- badCode
					return (badCodeInString !! 0)
					--let listOfLines = splitOn "\n" badCodeInString
					--return listOfLines
					--let listOfLinesAsStrings = listOfLines
					--return listOfLinesAsStrings
{-
startConvert = do
				textString <- badCode
				let result = convertToList (textString)
				return textString
-}			
--the book was called "learn you a haskell"
startConvert2 = do
					textString <- badCode
					let textLines = lines textString
					let c = textLines !! 0 ++ textLines !! 1
					return c 

--convertToList :: [[Char]] -> [[Char]]
{-
convertToList (x:xs) = 	if x == "\n" then 
							convertToList (xs)
						else
							do
								let [line:lines] = convertToList (xs)
								let newResult = [x:line] ++ lines
								return newResult
								
convertToList x = [[x]] -- x --[x, ' '] --(x !! 0)
-}
					
--packingTest = T.splitOn(T.pack "\n")(parseFileText)
					
-- find all the main functions
findFunctions :: [[Char]] -> Int
findFunctions (x:xs) = if "\tprivate int func1(int inputParam){" == x then
							0 -- return xs
						else 
							1 + findFunctions (x:xs)
--findFunctions x = "found nothing"