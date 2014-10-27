import Data.List.Split
import Text.Read
import qualified Data.Text as T

--get the code
badCode = readFile ("./se2s03/BadCode.java")
					
startConvert2 = do
					textString <- badCode
					let textLines = lines textString
					return textLines

--the book was called "learn you a haskell"
startConvert = do
	textString <- badCode
	let textLines = lines textString
	let indexOfStartOfFirstFunction = findFunctions textLines
	let reachEndOfFunction = findNextReturnStatment (textLines)(indexOfStartOfFirstFunction)
	let getReturningVariable = returningVariable (reachEndOfFunction)
	return reachEndOfFunction 
					

--
returningVariable (x:xs) = 	if x == " " then
								returningVariable (xs)
							else if 

-- find all the main functions
--findFunctions :: [[Char]] -> Int
findFunctions (x:xs) = if lineContains (x)("func") && lineContains(x)("private") then
							0 -- return xs
						else
							1 + findFunctions (xs)

findNextReturnStatment (list) index = 	if lineContains(list !! (index)) ("return") then
											index -- return xs
										else
											findNextReturnStatment (list)(index + 1)					

--
lineContains (x:xs) (y:ys) = 	if x == y then
									lineContains(xs)(ys)
								else
									lineContains(xs)(y:ys)
lineContains _ "" = True	
lineContains "" (y:ys) = False