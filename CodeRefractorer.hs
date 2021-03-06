import Language.Java.Lexer
main = startConvert

listOfFunctionNames = ["func1", "func2", "func3", "func4", "func5", "func6"]

--get the code
badCode = readFile ("./se2s03/BadCode.java")
			
--learn you a haskelll for gread good			
lexerTest text = lexer text	

--starts off by running this, (use main)
startConvert =
	do
		textString <- badCode
		let line:lines = reverse $ convertToListOfLines $ getFunctionCode ("func1") (lexerTest (textString))
		let line2:lines2 = convertToListOfLines(lexerTest(textString))
		return $ getRidOfBlocks (line2:lines2) (removeEmptyLoops $ reverse $ removeUnusedVariables(line:lines) ([getToken((lines !! 0) !! 1)]))
--------------------------------start of get rid of blocks---------------------------------

--this part together just goes through the code and replaces the variables in the loop with the global variables
getRidOfBlocks(allLines)(l:ls) = startGettingRidOfBlocks (l:ls) $ addLocalVariables $ valuesOfGlobalVariables $ drop 1 $ findFunction2 (IdentTok "badCode") (allLines)

--add local variables;
--the input is the valuesofglobalvariables
addLocalVariables x = x

--evaluateExpression x = 

--startGettingRidOfBlocks :: [[L Token]] -> [[L Token]] -> [[L Token]]
startGettingRidOfBlocks (line:lines) (vars)= 
	if lineContainsLoop(line) then 
		[handleIfStatement(line)(vars)] ++ startGettingRidOfBlocks(lines)(vars)
	else 
		[line] ++ startGettingRidOfBlocks(lines)(vars)
startGettingRidOfBlocks (emptyList)(vars) = []

--first replace the vars
handleIfStatement(l:ls)(vars) = replaceTheVar(l)(vars) ++ handleIfStatement(ls)(vars)
handleIfStatement(emptyList)(vars) = []
--replace 
replaceTheVar(l)(var:vars) = 
	if (getToken(l) == getToken(var !! 0)) then
		(drop 1 (var)) ++ replaceTheVar(l)(vars)
	else if (getToken(l) == KW_This || getToken(l) == Period)then 
		[]--remove the this.period if its there
	else
		replaceTheVar(l)(vars)
replaceTheVar(l)(emptyList) = [l]

valuesOfGlobalVariables :: [[L Token]] -> [[L Token]]
valuesOfGlobalVariables (line:lines) =
	if globalVariableWithValue (line) == [] then
		[]
	else
		[globalVariableWithValue (line)] ++ valuesOfGlobalVariables lines
valuesOfGlobalVariables emptyList = emptyList

globalVariableWithValue (l:ls) =
	if (getToken(l) == Op_Minus || isAnyVariable(getToken(l))) then [l] ++ globalVariableWithValue(ls)
	else if(getToken(l) == SemiColon || getToken(l) == Op_Equal) then
	globalVariableWithValue(ls)
	else []
globalVariableWithValue emptyList = []

--check if the input has a tok constructor
isAnyVariable (IntTok a) = True
isAnyVariable (LongTok a) = True
isAnyVariable (DoubleTok a) = True
isAnyVariable (FloatTok a) = True
isAnyVariable (CharTok a) = True
isAnyVariable (StringTok a) = True
isAnyVariable (BoolTok a) = True
isAnyVariable (IdentTok a) = True
isAnyVariable x = False

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
---------------------------------end of get rid of blocks-----------------------------------


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
{-
getValue :: (L a) -> a
getValue (IntTok a) = a
getValue (LongTok a) = a
getValue (DoubleTok a) = a
getValue (FloatTok a) = a
getValue (CharTok a) = a
getValue (StringTok a) = a
getValue (BoolTok a) = a
getValue (IdentTok a) = a
-}