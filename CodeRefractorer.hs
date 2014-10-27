--import Data.List.Split
--import Text.Read
--import qualified Data.Text as T
import Language.Java.Lexer

main = startConvert

--get the code
badCode = readFile ("./se2s03/BadCode.java")
			
--learn you a haskelll for gread good			
lexerTest text = lexer text	
{-
startConvert =
	do 
		textString <- badCode
		return $ refactorFunctionCode $ reverse $ convertToListOfLines $ getFunctionCode ("func1") (lexerTest (textString))

refactorFunctionCode (line:lines) = 
	getRidOfBlocks (line:lines) (removeEmptyLoops $ reverse $ removeUnusedVariables(line:lines) ([getToken((lines !! 0) !! 1)]))
-}
startConvert =
	do
		textString <- badCode
		let line:lines = reverse $ convertToListOfLines $ getFunctionCode ("func1") (lexerTest (textString))
		let line2:lines2 = convertToListOfLines(lexerTest(textString))
		return $ getRidOfBlocks (line2:lines2) (removeEmptyLoops $ reverse $ removeUnusedVariables(line:lines) ([getToken((lines !! 0) !! 1)]))
----------------------------start of get rid of blocks---------------------------------

getRidOfBlocks(allLines)(l:ls) = valuesOfGlobalVariables $ findFunction2 (IdentTok "badCode") (allLines)

valuesOfGlobalVariables (line:lines) =
	globalVariableWithValue (line) ++ valuesOfGlobalVariables lines
valuesOfGlobalVariables emptyList = emptyList

globalVariableWithValue (l:ls) =
	if (isVariable(getToken(l))) then [getToken(l)] ++ 
	
getValue(IdentTok name) rest = [name]
getValue(Op_Minus) (x:xs) = [-1 * getValue(x)]

--find values of global variables
--valuesOfGlobalVariables (line:lines) = getFunctionCode2 ("badCode") (line:lines)

--takes in a list of lines instead of the normal list
--getFunctionCode2 (name)(line:lines) = lines--findReturn2 $ findFunction2 name (x:xs)
--finds the return statement and returns everything in the middle

----finds the function declaration with the name equal to the variable
findFunction2 (name)(lines) =
	let p = findLineContainingVar (lines)(name)
		in take (findEndOfFunc (p)(lines)) (drop (p) (lines))

findLineContainingVar2 (line:lines) (name) =
	if ( lineContains (line) (name)) then 0 else 1 + findLineContainingVar2 (lines)(name)
		
--findEndOfFunc :: Int -> Int
findEndOfFunc startPos lines = findEndOfFunc2 (drop startPos lines)(1)
findEndOfFunc2 (line:lines)(0) = 0
findEndOfFunc2 (line:lines)(total) =
	if lineContains (line)(OpenCurly) then
		1 + findEndOfFunc2 (lines)(total)
	else if lineContains (line)(CloseCurly) then
		1 + findEndOfFunc2 (lines)(total-1)
	else 1 + findEndOfFunc2 (lines)(total) 
{-findFunction (name)(x:xs) =
	if getToken(x) == IdentTok(name) then 
		if getToken(xs!!1) == KW_Int then 
			[x] ++ xs
		else
			findFunction (name)(xs)
	else 
		(findFunction (name)(xs))
findFunction (name)(y) = []-}
------------------------------end of get rid of blocks-----------------------------------


--remove Empty Loops
----start functions for remove empty loops----
removeEmptyLoops (line:lines) = 
	if (lineContainsLoop (line)) then 
		 removeLoopIfEmpty (line:lines)
	else [line] ++ removeEmptyLoops (lines)
removeEmptyLoops emptyList = []

removeLoopIfEmpty (line:lines) = if (getToken((lines !! 0) !! 0) == CloseCurly) then removeEmptyLoops(drop 1 $ lines) else [line] ++ removeEmptyLoops(lines) 

--lineContains (l:ls)(KW_OpenCurly)
findOpenBrackets (l:ls) = if getToken(l) == OpenCurly then ls else findOpenBrackets (ls)
findOpenBrackets emptyList = emptyList
----done functions for remove emtpy loops----
		
lineContainsLoop (l:ls) = if (getToken(l) == KW_If || getToken(l) == KW_Else || getToken(l) == KW_While || getToken(l) == KW_For) then True else lineContainsLoop (ls)
lineContainsLoop (emptyList) = False

 
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
					let lexedLines = convertToListOfLines(lexerTest (textString))
					return lexedLines		
startConvert3 =
	do 
		textString <- badCode
		return $ reverse $ convertToListOfLines $ getFunctionCode ("func1") (lexerTest (textString))