import Language.Java.Lexer
import Prelude 
import Data.Char

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

main2 name =
	do
		textString <- badCode
		let allLines = convertToListOfLines $ lexerTest (textString)
		let globalVariables = getGlobalVariables (allLines) (getGlobalVariablesNames allLines)
		let newFunctionLines = getFunctionCode2 (name) allLines
		let functionLines = findAndReplaceAllGlobalVariables (newFunctionLines)(globalVariables)
		let (line:lines) = reverse $ findAndReplaceAllGlobalVariables(newFunctionLines)(globalVariables)
		let localVariables = findLocalVariables (functionLines)
		let newAnalyzedLines = (removeVariableDeclaration $ removeEmptyLoops $ reverse $ removeUnusedVariables(line:lines) ([getToken((lines !! 0) !! 1)]))
		return $ startParsingTheData (localVariables)(drop 1 newAnalyzedLines)

main =
	do
		textString <- badCode
		let allLines = convertToListOfLines $ lexerTest (textString)
		let globalVariables = getGlobalVariables (allLines) (getGlobalVariablesNames allLines)
		let newFunctionLines = getFunctionCode2 ("func5") allLines
		let functionLines = findAndReplaceAllGlobalVariables (newFunctionLines)(globalVariables)
		let (line:lines) = reverse $ findAndReplaceAllGlobalVariables(newFunctionLines)(globalVariables)
		let localVariables = findLocalVariables (functionLines)
		let newAnalyzedLines = (removeVariableDeclaration $ removeEmptyLoops $ reverse $ removeUnusedVariables(line:lines) ([getToken((lines !! 0) !! 1)]))
		return $ startParsingTheData (localVariables)(drop 1 newAnalyzedLines)
		
getReturn2 name = 
	do 
		result <- main2 name
		return $ convertListToString $ result
		
getReturn name = 
	do
		result <- startConvert name
		return $ convertListToString result

namesOfFunctions = ["func1", "func2", "func3", "func4", "func5", "func6"]
startConvert name =
	do
		textString <- badCode
		let allLines = convertToListOfLines $ lexerTest (textString)
		let globalVariables = getGlobalVariables (allLines) (getGlobalVariablesNames allLines)
		let functionLines = findAndReplaceAllGlobalVariables (convertToListOfLines $ getFunctionCode (name) (lexerTest (textString)))(globalVariables)
		let (line:lines) = reverse $ findAndReplaceAllGlobalVariables(functionLines)(globalVariables)-- $ functionLines
		let localVariables = findLocalVariables (functionLines)
		let newAnalyzedLines = (removeVariableDeclaration $ removeEmptyLoops $ reverse $ removeUnusedVariables(line:lines) ([getToken((lines !! 0) !! 1)]))
		return $ startParsingTheData (localVariables)(drop 1 newAnalyzedLines)--startParsingTheData(localVariables) $ drop 1 newAnalyzedLines

{-startConvert =
	do
		textString <- badCode
		let allLines = convertToListOfLines $ lexerTest (textString)
		let globalVariables = getGlobalVariables (allLines) (getGlobalVariablesNames allLines)
		let functionLines = findAndReplaceAllGlobalVariables (convertToListOfLines $ getFunctionCode ("func1") (lexerTest (textString)))(globalVariables)
		let (line:lines) = reverse $ findAndReplaceAllGlobalVariables(functionLines)(globalVariables)-- $ functionLines
		let localVariables = findLocalVariables (functionLines)
		let newAnalyzedLines = (removeVariableDeclaration $ removeEmptyLoops $ reverse $ removeUnusedVariables(line:lines) ([getToken((lines !! 0) !! 1)]))
		return $ startParsingTheData (localVariables)(drop 1 newAnalyzedLines)--startParsingTheData(localVariables) $ drop 1 newAnalyzedLines-}
----------------------------actually get rid of all the lines
{- this is how this is going to work, its going to keep replacing the value of the next variable with the pervious value of the variable
-}

removeVariableDeclaration(line:lines) = if getToken(line!!0) == KW_Int || getToken(line!!0) == KW_Double then removeVariableDeclaration(lines) else [line] ++ removeVariableDeclaration(lines)
removeVariableDeclaration(emptyList) = []

--makes the entire function just a return statement
startParsingTheData (localVariables)(line:lines)
	| containsBool(line) = startParsingTheData(localVariables)(handleIfStatements(localVariables)(newLine)(lines))--startParsingTheData(localVariables)(handleIfStatements(localVariables)(drop 1  $ take (length newLine - 3) $ ls)(lines))
	| lineContains(line)(Op_Equal) = startParsingTheData (newVars)(lines)
	| lineContains(line)(KW_Return) = drop 1 $ take (length newLine -1) newLine--replaceAllVariables(ls)(localVariables) 
	| otherwise = startParsingTheData(localVariables)(lines)---its a function declaration, brackets, class declaration, package
	where (l:ls) = line
	      newLine = [l] ++ replaceAllVariables(ls)(localVariables)
	      newVars = getNewVariables(newLine)(localVariables)
startParsingTheData (vars)(emptyList) = []

--is given the input, no barckets, no if or curly brackets, just converts it to True or False, it does this by converting it to a string first then a bool
handleIfStatements(localvariables)(l:ls)(lines) = deleteBlock(startConvertToBool2(l:ls))(lines)
	--if startConvertToBool(l:ls) then deleteBlock(False)(lines)
	--else deleteBlock(True)(lines)

-- False = delete the current if statement block
-- True = delete the next else block
deleteBlock(False)(line:lines) --deleting the if block
	| lineContains(line)(CloseCurly) && lineContains(lines!!0)(KW_Else) = (take (endBracketLocation-1) $ drop 1 lines) ++  (drop (endBracketLocation+1) lines)
	| lineContains(line)(CloseCurly) = lines--drop endBracketLocation lines
	| otherwise = deleteBlock(False)(lines)
	where endBracketLocation = findCloseBracket(drop 1 lines)
deleteBlock(True)(line:lines) --deleting the else block
	| lineContains(line)(CloseCurly) && lineContains(lines!!0)(KW_Else) = drop (ebl+1) (lines) 
	| lineContains(line)(CloseCurly) = lines --take (ebl-1) lines ++ drop (eblOfElse + ebl) (lines)  
	| otherwise = [line] ++  deleteBlock(True)(lines)
	where ebl = findCloseBracket(drop 1 lines)
	      eblOfElse = findCloseBracket(drop (ebl+1) lines)
		  --ebl = end bracket location 
deleteBlock(n)(emptyList) = [] --should never get called

findCloseBracket(lines) = findCloseBracket2(lines)(0)
findCloseBracket2(line:lines)(n)
	| lineContains (line)(CloseCurly) && n == 0 = 1
	| lineContains (line)(CloseCurly) && n /= 0 = 1+ findCloseBracket2(lines)(n-1)
	| lineContains (line)(OpenCurly) = 1+findCloseBracket2(lines)(n+1)
	| otherwise = 1 + findCloseBracket2(lines)(n)
findCloseBracket2(emptyList)(n) = 0

locateBoolOperator (l:ls)
	| getToken(l) == Op_Equal = 0
	| getToken(l) == Op_GThan = 0
	| getToken(l) == Op_LThan = 0
	| otherwise = 1+locateBoolOperator (ls)

startConvertToBool2 (l:ls) = convertToBool2(take pos insideBracket)(insideBracket !! pos)(drop (pos+1) (insideBracket))
	where insideBracket = startConvertToBool3(l:ls)(False)
	      pos = locateBoolOperator(insideBracket)
	
startConvertToBool3(l:ls)(found)
	| getToken(l) == CloseParen && found = []
	| getToken(l) == OpenParen && not found = startConvertToBool3(ls)(True)
	| found = [l] ++ startConvertToBool3(ls)(found)
	| not found = startConvertToBool3(ls)(found)
	| otherwise = [] -- should never get called
	
--ls = left side, op = operator, rs = right side
convertToBool2 :: [L Token] -> (L Token) -> [L Token] -> Bool
convertToBool2 (ls)(op)(rs)
	| getToken(op) == Op_Equals = (getListValue(ls) == getListValue(rs)) 
	| getToken(op) == Op_GThan = (getListValue(ls) > getListValue(rs))
	| getToken(op) == Op_LThan = (getListValue(ls) < getListValue(rs))
------------------ALL this is wrong--------------
--converts something like 4 < 5 to true or 3 > 4 to false
startConvertToBool (l1:l2:ls)
	| containsBool([l2]) = convertToBool([l1] ++ [l2] ++ [ls !! 0])
	| otherwise = {-[l1] ++ -} startConvertToBool(l2:ls)
startConvertToBool (l:ls) = False--l:ls
startConvertToBool (rest) = False--rest

convertToBool(x:y:z)
	| getToken(y) == Op_Equals = (getValue(x) == getValue(zt))
	| getToken(y) == Op_GThan = (getValue(x) > getValue(zt))
	| getToken(y) == Op_LThan = (getValue(x) < getValue(zt))
	| otherwise = False
	where xt = getToken(x)
	      zt = z!!0--getToken(z!!0)
-----------------done being wrong-------------
{-
convertToBool(x:y:z)
	| yt == Op_Equals 	= (xt == zt)
	| yt == Op_GThan 	= (xt > zt)
	| yt == Op_LThan 	= (xt < zt)
	| otherwise = False
	where xt = getValue(getToken(x))
	      yt = getToken(y)
	      zt = getValue(getToken(z))
-}
	
--l = variables, ls !!0 = '=', drop 1 ls = the value of the variable + ;
--takes the new variables and adjusts the variables list accordingly so the varaibles are updated
--getNewVariables (l:ls)(variables) = take(pos)(variables) ++ [[l] ++ (take(length ls -2) $ drop 1 ls)] ++ drop (pos+1) variables
	--where pos = isIn2DList(l)(variables)
--getNewVariables :: [L Token] -> [L Token]
getNewVariables (l:ls)(variables) = take(pos)(variables) ++ [[l] ++ newVarValue] ++ drop(pos+1)(variables)
	where pos = isIn2DList(l)(variables)
	      newVarExpression = replaceAllVariables(ls)(variables)
	      newVarValue = removeSyntax(newVarExpression)-- [L(0,0)(OpenParen)] ++ removeSyntax(newVarExpression) ++ [L(0,0)(CloseParen)]
		  
		
--removes the syntax and just keeps the expression
removeSyntax :: [L Token] -> [L Token]
removeSyntax (l:ls)
	| isVariable(getToken(l)) = [l] ++ removeSyntax(ls)
	| getToken(l) == Op_Plus || getToken(l) == Op_Minus = [l] ++ removeSyntax(ls)
	| isInt(l) || isDouble(l) = [l] ++ removeSyntax(ls)
	| getToken(l) == OpenParen || getToken(l) == CloseParen = [l] ++ removeSyntax(ls)
	| otherwise = removeSyntax (ls)
removeSyntax(emptyList) = emptyList 
	
--this replaces all the values of the variables in the expression to the variables values	
replaceAllVariables(l:ls)(variables)
	| pos == -1 = [l] ++ replaceAllVariables(ls)(variables)
	| isAnyVariable(getToken(l)) =  [L(0,0)(OpenParen)] ++ (drop 1 $ (variables !! pos)) ++ [L(0,0)(CloseParen)] ++ replaceAllVariables(ls)(variables)
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
	

getFunctionCode2 :: String -> [[L Token]] -> [[L Token]]
getFunctionCode2 (name)(lines) = take (endFuncPos+1) (newLines)
	where newLines = findFunction3 (name)(lines)
	      endFuncPos = findCloseBracket(drop 1 newLines)
		  
--finds the function and returns the rest
findFunction3 :: String -> [[L Token]] -> [[L Token]]
findFunction3 (name)(line:lines) =
	if lineContains(line)(IdentTok name) && lineContains(line)(KW_Private) then line:lines
	else findFunction3 (name)(lines)
findFunction3 (name)(emptyList) = []

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
---------------------used to convert stuff back to normal values

getDoubleValue :: (L Token) -> Double
getDoubleValue (L pos (DoubleTok x)) = x
getIntValue :: (L Token) -> Integer
getIntValue (L pos (IntTok x)) = x

isInt (L pos (IntTok x)) = True
isInt (L pos y) = False
isDouble (L pos (DoubleTok x)) = True
isDouble (L pos y) = False

hasValue(x)
	| isInt(x) = True
	| isDouble(x) = True
	| otherwise = False

getValue(x)
	| isDouble(x) = getDoubleValue(x)
	| isInt(x) = fromIntegral( getIntValue(x) ) + 0.0
	| otherwise = 0
	
--getListValue :: [L Token] -> Double
getListValue (x:y:rst) = -getValue(y)
getListValue (x:xs) = getValue(x)
getListValue (emptyList) = 0
{-
getListValue :: [L Token] -> Double -> Double
getListValue(y:ys)(lastValue)
	| hasValue(y) = getListValue(ys)(getValue(y))
	| tokenY == Op_Minus = getListValue(ys)(-lastValue)
	| tokenY == Op_Plus = lastValue + getListValue(ys)(lastValue)
	| tokenY == Op_Star = lastValue * getListValue(ys)(lastValue)
	| tokenY == Op_Slash = lastValue / getListValue(ys)(lastValue)
	| otherwise = 0
	where tokenY = getToken(y)
	-}
------------------------------------------used to convert things back to int and double and negative
convertListOfListsToString :: [[L Token]] -> String
convertListOfListsToString (line:lines) = convertListToString(line) ++ convertListOfListsToString (lines)
convertListOfListsToString (emptyList) = []

convertListToString :: [L Token] -> String
convertListToString (l:ls) = convertToString (l) ++ " " ++ convertListToString(ls)
convertListToString (emptyList) = []

convertToString :: (L Token) -> String
convertToString (L (p)(OpenParen)) = "("
convertToString (L (p)(CloseParen)) = ")"
convertToString (L (p)(Op_Plus)) = "+"
convertToString (L (p)(Op_Minus)) = "-"
convertToString (L (p)(SemiColon)) = ";"
convertToString (L (p)(IdentTok str)) = str
convertToString (L (p)(IntTok x)) = show x--getInts(fromIntegral(x))
convertToString (L (p)(DoubleTok x)) = show x--convertDoubleToString (x)(16)
convertToString (L (p)(FloatTok x)) = show x
{-
--made functions to convert ints and doubles to strings, turns out there
--is a built in function that does it for you called show
getInts :: Int -> String
getInts (x)
	| x > -1 && x < 10 = [(chr(fromIntegral(x) + ord('0')))]
	| x < 0 = "-" ++ getInts(-x)
	| x > 9 = getInts(quot x 10) ++ [(chr(fromIntegral(mod x 10) + ord('0')))]

convertDoubleToString :: Double -> Int -> String
convertDoubleToString (x)(n)
	| n == 0 = ""
	| x - fromIntegral(floor(x)) == 0 = getInts (fromIntegral(floor(x))) --contains no decimal
	| floor(x) == 0 = getInts(fromIntegral $ floor $ x*10) ++ convertDoubleToString (x)(n-1)
	| otherwise = "asd"
	-}