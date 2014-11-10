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
		let functionLines = convertToListOfLines $ getFunctionCode ("func1") (lexerTest (textString))
		let (line:lines) = reverse $ functionLines
		--let line2:lines2 = convertToListOfLines(lexerTest(textString))
		let globalVariables = valuesOfGlobalVariables $ drop 1 $ findFunction2 (IdentTok "badCode") (functionLines)
		--return (removeEmptyLoops $ reverse $ removeUnusedVariables(line:lines) ([getToken((lines !! 0) !! 1)]))
		return $ getRidOfBlocks (globalVariables) (removeEmptyLoops $ reverse $ removeUnusedVariables(line:lines) ([getToken((lines !! 0) !! 1)]))
----------------------------start of get rid of blocks---------------------------------

getRidOfBlocks(globalVariables)(line:lines) = replaceGlobalVariables(lines)(globalVariables)--replaceAllBools(globalVariables)(replaceGlobalVariables(lines))
--getRidOfBlocks(allLines)(l:ls) = valuesOfGlobalVariables $ drop 1 $ findFunction2 (IdentTok "badCode") (allLines)

--finds the line that contains a global variable and replaces it
replaceGlobalVariables (line:lines)(globalVariables)
	| lineContains(line)(KW_This) = replaceGlobalVariables(line)(globalVariables) ++ replaceGlobalVariables(lines)(globalVariables)
	| otherwise = replaceGlobalVariables(lines)(globalVariables)
replaceGlobalVariables (emptyList)(globalVariables) = []

replaceGlobalVariables (l:ls)(globalVariables)
	| (lineContains(l:ls)(KW_This)) = replaceVariableWith(l:ls)(globalVariables) -- ++ replaceGlobalVariables(lines)(globalVariables)
	| otherwise = [line] ++ replaceGlobalVariables(lines)(globalVariables)
replaceGlobalVariables(emptyList)(_) = []

--l is the variable and it replaces it with the correct variable
replaceVariableWith (l) (var:vars) = [l]{-
	|  pos > -1 = drop 1 ((var:vars) !! pos)
	| otherwise = replaceVariableWith(l)(vars)
	where 	pos = findLineContainingVar(var)(l) -}
replaceVariableWith(l)(emptyList) = []

replaceAllBools(gVar:gVars)(line:lines) = if containsBool(line) then replaceBool(line)(gVar:gVars)(getLocalVariables(line:lines)) else replaceAllBools(gVar:gVars)(lines)--if lineContains(line)(Op_Equals) then line else replaceAllBools (var:vars)(lines) --if lineContainsList(line)(var) then line else replaceAllBools(var:vars)(lines)
replaceAllBools(var:vars)(emptyList) = []
replaceAllBools (l)(b) = [] -----------might get called------

--replaces the bool with an actual vakue
replaceBool(line)(gVar:gVars)(lVar:lVars) = replaceValues(line)(gVar:gVars)(lVar:lVars)
replaceBool(line)(emptyGVars)(emptyLVars) = []

--gv = global variables, gvs = global variables, lv = local variable, lvs = local variables 
--replace the variables in the if statement with the actual value of the variable
replaceValues(l:ls)(gv:gvs)(lv:lvs)
	| getToken(l) == KW_This = replaceGlobalValue(ls !! 1)(gv:gvs)
	| l == (lv !! 0) 		= drop 1 lv
	| x /= [] 				= x
	| otherwise 			= replaceValues(ls)(gv:gvs)(lv:lvs)
	where x = replaceValues(l:ls)(gv:gvs)(lvs) 
	
--replaceValues(noLines)()
replaceValues(noLines)(noGVars)(noLVars) = []
	
replaceGlobalValue(l)(gv:gvs)
	| getToken(l) == getToken(gv !! 0) = drop 1 gv
	| gvs == [] = []				--should never be returned
	| otherwise = replaceGlobalValue(l)(gvs)
--replaceGlobalValue(l)[] == [] --should never get called

--getValue :: (Integral a) => (L Token) -> a
{-getValue :: (Num a) => (L a) -> a
getValue (L pos x)
	| isDouble(x) = getDoubleValue(x)
	| isInt(x) = getIntValue(x){-
	| isFloatValue(x) = getFloatValue(x)
	| isLongValue(x) = getLongValue(x)-}
	| otherwise = 0
	-}
getDoubleValue (L pos (DoubleTok x)) = x
getIntValue (L pos (IntTok x)) = x

isInt (L pos (IntTok x)) = True
isInt (L pos y) = False
isDouble (L pos (DoubleTok x)) = True
isDouble (L pos y) = False


getLocalVariables (line:lines) = if ((lineContains(line)(KW_Double) || lineContains(line)(KW_Int)) && lineContains(line)(Op_Equal)) then [globalVariableWithValue(line)] ++ getLocalVariables(lines) else getLocalVariables(lines)
getLocalVariables (emptyList) = []

--localVariableWithValue(l:ls) = 

valuesOfGlobalVariables :: [[L Token]] -> [[L Token]]
valuesOfGlobalVariables (line:lines) =
	if globalVariableWithValue (line) == [] then
		[] -- ++ valuesOfGlobalVariables lines
	else
		[globalVariableWithValue (line)] ++ valuesOfGlobalVariables lines
valuesOfGlobalVariables emptyList = emptyList

globalVariableWithValue (l:ls) =
	if (getToken(l) == Op_Minus || isAnyVariable(getToken(l))) then [l] ++ globalVariableWithValue(ls)
	else if(getToken(l) == SemiColon || getToken(l) == Op_Equal || getToken(l) == KW_Double || getToken(l) == KW_Int) then 
	globalVariableWithValue(ls)--globalVariableWithValue (ls) 
	else []--[l] ++ globalVariableWithValue(ls)
globalVariableWithValue emptyList = []

isAnyVariable (IntTok a) = True
isAnyVariable (LongTok a) = True
isAnyVariable (DoubleTok a) = True
isAnyVariable (FloatTok a) = True
isAnyVariable (CharTok a) = True
isAnyVariable (StringTok a) = True
isAnyVariable (BoolTok a) = True
isAnyVariable (IdentTok a) = True
isAnyVariable x = False
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
findEndOfFunc2 (emptyList)(total) = 0 ----------------should never get called
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