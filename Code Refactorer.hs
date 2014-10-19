import Data.List.Split
import Text.Read
import qualified Data.Text as T

--get the code
badCode = readFile ("./se2s03/BadCode.java")
parseFileText = do 
					badCodeInString <- badCode
					return badCodeInString
					--let listOfLines = splitOn "\n" badCodeInString
					--return listOfLines
					--let listOfLinesAsStrings = listOfLines
					--return listOfLinesAsStrings

packingTest = T.splitOn(T.pack "\n")(parseFileText)
					
-- find all the main functions
findFunctions :: [[Char]] -> Int
findFunctions (x:xs) = if "\tprivate int func1(int inputParam){" == x then
							0 -- return xs
						else 
							1 + findFunctions (x:xs)
--findFunctions x = "found nothing"