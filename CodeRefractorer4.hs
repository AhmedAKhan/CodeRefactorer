import Data.List.Split
import Text.Read
import qualified Data.Text as T
import Language.Java.Lexer

--type Pos = (Int, Int)

--get the code
badCode = readFile ("./se2s03/BadCode.java")
			
--learn you a haskelll for gread good			
lexerTest text = lexer text	

startConvert =
	do 
		textString <- badCode
		return $ refactorFunctionCode $ reverse $ convertToListOfLines $ getFunctionCode ("func1") (lexerTest (textString))

--x:xs is all the parts in the function func 1 (reversed)
--the returning variable = xs !! 1
--go up the code and remove all the variables that
--do not change the return statement
{-refactorFunctionCode (x:xs) =
	do 
		let returningVar = (xs !! 1)
		let usedVariables = [returningVar]
		let l:ls = convertToListOfLines(x:xs)
		let result = lineContains ([])(usedVariables)
		result=
-}
refactorFunctionCode (line:lines) = 
	removeUnusedVariables (line:lines)([getToken((lines !! 0) !! 1)])
	--let l:ls = convertToListOfLines (x:xs)
		--in lineContainsList (drop 1 l) [getToken(xs !! 1)]
	
--checkIfLine is a statement
removeUnusedVariables(line:lines)(vars) =
	if ((not $ doesLineReassign(line)(vars)) && (isLineAStatement $ line)) then
		removeUnusedVariables(lines)(vars)
	else
		[line] ++ removeUnusedVariables(lines)(getNewVars(vars)(line))
removeUnusedVariables(emptyList)(vars) = []

--update the vars based on the given expression
getNewVars (vars)(l:ls) = 
	if (isVariable $ getToken $ l) then
		getNewVars(vars)(ls) ++ [getToken(l)]
	else
		getNewVars(vars)(ls)
getNewVars (vars)(emptyList) = vars

isVariable (IdentTok a) = True
isVariable x = False

doesLineReassign(l:ls)(vars) =
	if isContainedInList (getToken(l)) (vars) then True
	else if getToken(l) == Op_Equal then False
	else doesLineReassign (ls)(vars)
doesLineReassign(emptyList)(vars) = False

isContainedInList (t)(var:vars) =
	if t == var then 
		True 
	else
		isContainedInList(t)(vars)
isContainedInList (t)(emptyList) = False

isLineAStatement (x:xs) = 
	if getToken(x) == Op_Equal then True else isLineAStatement(xs)
isLineAStatement (x) = False 
	
removeLineFromList (lines)(indexOfItemToRemove) =
	 (take (indexOfItemToRemove-1) (lines))++ (drop indexOfItemToRemove $ lines)

--lineContainsList :: Eq a => [L a] -> [a] -> Bool
lineContainsList (line)(var:vars) = 
	if lineContains(line)(var) then
		True
	else 
		lineContainsList(line)(vars)
lineContainsList (line)(emptyList) = False

--finds the line containing the variable and returns the index of where that occurs in the list
findLineContainingVar (line:lines)(var) =
	if lineContains (line)(var) then
		0
	else 
		1 + findLineContainingVar (lines)(var)
findLineContainingVar (line)(var) = -1

--checkes if the line given contains the variable given
lineContains (x:xs)(var)=
	if getToken(x) == var then
		True
	else 
		lineContains (xs)(var)
lineContains (x)(var) = False

convertToListOfLines (x:xs) = 
	let line = combineTheLines(x:xs)
		in [line] ++ (convertToListOfLines( drop ((length line)-1) (xs)))
convertToListOfLines (emptyList) = [] 
--combines the lines into sentneces
combineTheLines2 (lineNumber)(x:xs) = 
	if (getLineNumber(getPos(x)) == lineNumber) then
		[x] ++ combineTheLines2(lineNumber)(xs)
	else
		[] 
combineTheLines2 (lineNumber) (y) = []
combineTheLines(y) = combineTheLines2 (getLineNumber $ getPos $ (y!!0))(y)



--returns all the code in a function declaration
getFunctionCode (name) (x:xs) = 
	findReturn $ findFunction name (x:xs)
		
--finds the return statement and returns everything in the middle
findReturn (x:xs) = 
	if getToken(x) == KW_Return then 
		[x] ++ (take 3 xs)
	else
		[x] ++ findReturn(xs)

--finds the function declaration with the name equal to the variable
findFunction (name)(x:xs) =
	if getToken(x) == IdentTok(name) then 
		if getToken(xs!!1) == KW_Int then 
			[x] ++ xs
		else
			findFunction (name)(xs)
	else 
		(findFunction (name)(xs))
findFunction (name)(y) = []

--used for getting arguments of the pos in L
getLineNumber (x, _) = x
getCharacterNumber(_, x) = x

--get the token, or position from the L data type
getToken (L _ y) = y
getPos (L y _) = y



--used for testing
startConvert2 = do
					textString <- badCode
					let lexedLines = lexerTest textString
					return lexedLines		
startConvert3 =
	do 
		textString <- badCode
		return $ reverse $ convertToListOfLines $ getFunctionCode ("func1") (lexerTest (textString))