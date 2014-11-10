import Language.Java.Lexer
--import Language.Haskell.Interpretter.GHC

main = startConvert

--get the code
badCode = readFile ("./se2s03/BadCode.java")
			
--learn you a haskelll for gread good			
lexerTest text = lexer text	

{- in order what this start convert is going to do
1. get the code, convert it to a list of lines, 
2. get the function 1 lines
3. get all the global variables and replace their values with everything in the code
4. get the local variables, and parse all the if statements
5. keep repeatedly take the values of the take the return of the function and replace it with the value of the that variable

6. repeat for all functions
-}
startConvert =
	do
		textString <- badCode
		let allLines = convertToListOfLines $ lexerTest (textString)
		let globalVariables = getGlobalVariables (allLines) (getGlobalVariablesNames allLines)
		let functionLines = findAndReplaceAllGlobalVariables (convertToListOfLines $ getFunctionCode ("func1") (lexerTest (textString)))(globalVariables)
		let (line:lines) = reverse $ findAndReplaceAllGlobalVariables(functionLines)(globalVariables)-- $ functionLines
		let localVariables = findLocalVariables (functionLines)
		let newAnalyzedLines = (removeVariableDeclaration $ removeEmptyLoops $ reverse $ removeUnusedVariables(line:lines) ([getToken((lines !! 0) !! 1)]))
		return $ startParsingTheData (localVariables)(drop 1 newAnalyzedLines)--startParsingTheData(localVariables) $ drop 1 newAnalyzedLines
----------------------------actually get rid of all the lines
{- this is how this is going to work, its going to keep replacing the value of the next variable with the pervious value of the variable
-}

removeVariableDeclaration(line:lines) = if getToken(line!!0) == KW_Int || getToken(line!!0) == KW_Double then removeVariableDeclaration(lines) else [line] ++ removeVariableDeclaration(lines)
removeVariableDeclaration(emptyList) = []

startParsingTheData (localVariables)(line:lines)
	| containsBool(line) = handleIfStatements(localVariables)(drop 1  $ take (length newLines - 3) $ ls)-- got rid of the if and ( and the { and )
	| lineContains(line)(Op_Equal) = startParsingTheData (newVars)(lines)
	| lineContains(line)(Op_Equals) = [line]
	| lineContains(line)(KW_Return) = [line]
	| otherwise = [line] ++ startParsingTheData(localVariables)(lines)---its a function declaration, brackets, class declaration, package
	where (l:ls) = line
	      newLines = [l] ++ replaceAllVariables(ls)(localVariables)
	      newVars = getNewVariables(newLines)(localVariables)
startParsingTheData (vars)(emptyList) = []

--is given the input, no barckets, no if or curly brackets, just converts it to True or False, it does this by converting it to a string first then a bool
handleIfStatements(localvariables)(l:ls) = []
	--| getToken(l)
{-handleIfStatements(localVariables)(l:ls)
	| getToken(l) == Op_Minus = convertNegative(l) * handleIfStatements(localVariables)(ls)
	| getToken(l) == DoubleTok = getDoubleValue(l) 
	| otherwise = getDoubleValue(l)-}
	
	
--l = variables, ls !!0 = '=', drop 1 ls = the value of the variable + ;
--takes the new variables and adjusts the variables list accordingly so the varaibles are updated
getNewVariables (l:ls)(variables) = take(pos)(variables) ++ [[l] ++ (take(length ls -2) $ drop 1 ls) ] ++ drop (pos+1) variables
	where pos = isIn2DList(l)(variables)

--this replaces all the values of the variables in the expression to the variables values	
replaceAllVariables(l:ls)(variables)
	| pos == -1 = [l] ++ replaceAllVariables(ls)(variables)
	| isAnyVariable(getToken(l)) = (drop 1 $ (variables !! pos)) ++ replaceAllVariables(ls)(variables)
	| otherwise = [l] ++ replaceAllVariables(ls)(variables)
	where pos = isIn2DList(l)(variables)
replaceAllVariables(emptyList)(variable) = [] 


lineContainsVar (l:ls) = if isAnyVariable(getToken(l)) then True else lineContainsVar(ls)
lineContainsVar (emptyList) = False


----------------------------end of actually getting rid of all the lines
		
		
----------------------------start of get rid of blocks---------------------------------

getRidOfBlocks(localVariables)(line:lines) = replaceAllBools(localVariables)(line:lines)
-------------------------------start of replace all global variables with their value
--finds line that contains a global variable and replaces it with the value of the global variable
findAndReplaceAllGlobalVariables (line:lines)(globalVariables)
	| lineContains(line)(KW_This) = [findAndReplaceVariableInLine(line)(globalVariables)] ++ findAndReplaceAllGlobalVariables(lines)(globalVariables)
	| otherwise = [line] ++ findAndReplaceAllGlobalVariables(lines)(globalVariables)
findAndReplaceAllGlobalVariables (emptyList)(globalVariables) = []

--finds all the lines that contain a global variable and replaces them with the value of the global variable
findAndReplaceVariableInLine(l:ls)(globalVariables)
	| getToken(l) == KW_This 	= replaceVariableWith(ls !! 1)(globalVariables) ++ findAndReplaceVariableInLine(drop 2 ls)(globalVariables)
	| otherwise = [l] ++ findAndReplaceVariableInLine(ls)(globalVariables)
findAndReplaceVariableInLine(emptyList)(_) = []
	
--l is the variable and it replaces it with the correct variable
replaceVariableWith (l) (var:vars)
	|  getToken(l) == getToken(var!!0) = drop 1 var--pos > -1 = drop 1 ((var:vars) !! pos)
	| otherwise = replaceVariableWith(l)(vars)
	--where 	pos = findLineContainingVar(var)(l)
replaceVariableWith(l)(emptyList) = []
-------------------------------end of replace all global variables with their value

---------------------------start of replace bool with True or False

--replaceAllBools(line:lines)(localVariables) = if containsBool(line) then replaceBool(line)(localVariables) else replaceAllBools(lines)--if lineContains(line)(Op_Equals) then line else replaceAllBools (var:vars)(lines) --if lineContainsList(line)(var) then line else replaceAllBools(var:vars)(lines)
replaceAllBools(var:vars)(emptyList) = []
replaceAllBools (l)(b) = [] -----------might get called------

--replaces the bool with an actual vakue
replaceBool(l:ls)(lVar:lVars) = l:ls--replaceValues(line)(gVar:gVars)(lVar:lVars)
replaceBool(l:ls)(emptyLVars) = []
{-
--gv = global variables, gvs = global variables, lv = local variable, lvs = local variables 
--replace the variables in the if statement with the actual value of the variable
replaceValues(l:ls)(gv:gvs)(lv:lvs)
	| getToken(l) == KW_This = replaceGlobalValue(ls !! 1)(gv:gvs)
	| l == (lv !! 0) 		= drop 1 lv
	| x /= [] 				= x
	| otherwise 			= replaceValues(ls)(gv:gvs)(lv:lvs)
	where x = replaceValues(l:ls)(gv:gvs)(lvs) 
	
replaceValues(noLines)(noGVars)(noLVars) = []
	-}
---------------------------start of replace bool with True or False
	

replaceGlobalValue(l)(gv:gvs)
	| getToken(l) == getToken(gv !! 0) = drop 1 gv
	| gvs == [] = []				--should never be returned
	| otherwise = replaceGlobalValue(l)(gvs)
--replaceGlobalValue(l)[] == [] --should never get called

getDoubleValue (L pos (DoubleTok x)) = x
getIntValue (L pos (IntTok x)) = x

isInt (L pos (IntTok x)) = True
isInt (L pos y) = False
isDouble (L pos (DoubleTok x)) = True
isDouble (L pos y) = False

--isAnyVariable (IntTok a) = True
--isAnyVariable (LongTok a) = True
--isAnyVariable (DoubleTok a) = True
--isAnyVariable (FloatTok a) = True
--isAnyVariable (CharTok a) = True
--isAnyVariable (StringTok a) = True
--isAnyVariable (BoolTok a) = True
isAnyVariable (IdentTok a) = True
isAnyVariable x = False
--find values of global variables

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
findEndOfFunc2 (emptyList)(total) = 0 ----------------should never get called
------------------------------end of get rid of blocks-----------------------------------

-----------------------------start of getting local variables 
findLocalVariables (line:lines)= if fillInValueForLocalVariables(line) /= [] then [fillInValueForLocalVariables(line)] ++ findLocalVariables (lines) else findLocalVariables (lines)
findLocalVariables (emptyList) = []

fillInValueForLocalVariables (l:ls)
	| getToken(l) /= KW_Int && getToken(l) /= KW_Double = []
	| not $ isAnyVariable(getToken(ls !! 0)) = []
	| getToken(ls!!1) /= Op_Equal = []
	| otherwise =  [ls!!0] ++ (take (length ls - 3)(drop 2 ls))
fillInValueForLocalVariables emptyList = []
-----------------------------end of get local variables

-----------------------get global variables
getGlobalVariablesNames(line:lines)
	| l1 == KW_Final = getGlobalVariablesNames ( [drop 1 line] ++ lines)
	| l1 /= KW_Private && l1 /= KW_Public = getGlobalVariablesNames(lines)
	| l2 /= KW_Int && l2 /= KW_Double = getGlobalVariablesNames(lines)
	| length line == 4 = [[line !! 2]] ++  getGlobalVariablesNames(lines)
	where l1 = getToken(line !! 0)
	      l2 = getToken(line !! 1)
getGlobalVariablesNames(emptyList) = []

getGlobalVariables(line:lines)(globalVariables) =
	if lineContains(line)(IdentTok "func1") then globalVariables
	else getGlobalVariables(lines)(newGlobalVariables)
	where newGlobalVariables = fillInValueForGlobalVariables(line)(globalVariables) 
getGlobalVariables (emptyList)(globalVariables) = globalVariables

fillInValueForGlobalVariables (l:ls)(globalVariables)
	| pos == -1 = globalVariables
	| getToken(ls!!0) /= Op_Equal = globalVariables --l:ls ++ [L (0,0) (IdentTok "2")]
	| otherwise = take (pos) globalVariables ++ [(globalVariables !! pos) ++ rest] ++ drop (pos + 1) globalVariables
	where pos = isIn2DList(l)(globalVariables)
	      rest = take (length ls - 2) (drop 1 ls)
fillInValueForGlobalVariables emptyList globalVariables = globalVariables

--takes in an item in and checks if it is in the global variables and in which position
isIn2DList (item)(varIL:varsIL)
  | getToken(item) == var = 0
  | itemPosition == -1 = -1
  | otherwise = 1 + itemPosition
  where var = getToken(varIL !! 0)
        itemPosition = isIn2DList (item)(varsIL)
isIn2DList (item)(emptyList) = -1
------------------------------------get global variables



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

containsBool(line) 
	| lineContains(line)(Op_Equals) = True 
	| lineContains(line)(Op_GThan) = True
	| lineContains(line)(Op_LThan) = True
	| otherwise = False

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
		
------------------------------------------used to convert things back to int and double and negative
convert(L pos (Op_Minus)) = '-'
convert(L pos (DoubleTok a)) = show a
convert(L pos (IntTok a)) = show a
convert(L pos (FloatTok a)) = show a
convert(L pos (Op_Equals)) = '=='
convert(L pos (Op_GThan)) = '>'
convert(L pos (Op_LThan)) = '<'
convert(L pos Op_Plus) = '+'
convert(L pos (Op_Star)) = '*'

{-
convertNegative(L pos (Op_Minus)) = -1
convertDouble(L pos (DoubleTok a)) = a
convertInt(L pos (IntTok a)) = a
convertFloat(L pos (FloatTok a)) = a
convertSign(L pos (Op_Equals)) a b = a == b
convertSign(L pos (Op_GThan)) a b = a > b
convertSign(L pos (Op_LThan)) a b = a < b
-}