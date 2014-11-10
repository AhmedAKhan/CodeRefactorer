import Language.Java.Lexer
import Prelude 
import Data.Char
import System.IO

--get the code
badCode = readFile ("./se2s03/BadCode.java")

--this is a useless function, but I used it for debugging So I could manually just change the result of the lexer test here and see how it owuld effect my code			
lexerTest text = lexer text	 

--this creates an empty java program and fils in the contents that are passed in
createOutputFile content = 
	writeFile ("BadCode3.java") ("package se2s03;\npublic class BadCode {\n\tpublic double badCode(int var1, int var2, int var3, int var4, int var5, int var6) {\n\t\t" ++ content++"\n\t}\n}")
	
{- in order what this start convert is going to do
1. get the code, convert it to a list of lines, 
2. get the function 1 lines
3. get all the global variables and replace their values with everything in the code
4. get the local variables, and parse all the if statements
5. keep repeatedly take the values of the take the return of the function and replace it with the value of the that variable
6. repeat for all functions
-}
startConvert theCode name=
	do
		let textString = theCode
		let allLines = convertToListOfLines $ lexerTest (textString)
		let globalVariables = getGlobalVariables (allLines) (getGlobalVariablesNames allLines)
		let newFunctionLines = getFunctionCode2 (name) allLines
		let functionLines = findAndReplaceAllGlobalVariables (newFunctionLines)(globalVariables)
		let (line:lines) = reverse $ findAndReplaceAllGlobalVariables(newFunctionLines)(globalVariables)
		let localVariables = findLocalVariables (functionLines)
		let newAnalyzedLines = (removeVariableDeclaration $ removeEmptyLoops $ reverse $ removeUnusedVariables(line:lines) ([getToken((lines !! 0) !! 1)]))
		return $ startParsingTheData (localVariables)(drop 1 newAnalyzedLines)

--get what the function will return, name = the function name, theCode = the code of the whole program
getReturn theCode name = 
	do 
		result <- startConvert theCode name
		convertListToString $ result
		
--this will take all the returns and return it in a list
getAllReturns = 
	do 
		theCode <- badCode
		return $ (map (getReturn (theCode) )(namesOfFunctions))
		
--this is the main function which actually starts everything
main =
	do
		results <- getAllReturns
		let lexedLines = (map lexer) (results)
		let resultAsLexedLines = replaceInputParamOfLines(1)(lexedLines)
		let resultAsList = map(convertListToString)(resultAsLexedLines)
		let finalResult = combineAllStrings resultAsList
		createOutputFile ("return " ++ finalResult)
		return "starting.\n.\n.\n.done\nThank you for your patience"

--combines all the strings given into one string
combineAllStrings (line:lines) = line ++ combineAllStrings (lines)
combineAllStrings emptyList = []
		
-----------------converts all the inputParam variables into var1 or var -n depending on the function
replaceInputParamOfLines :: Int -> [[L Token]] -> [[L Token]]
replaceInputParamOfLines(n)(line:lines) = [replaceInputParam(n)(line)] ++ replaceInputParamOfLines(n+1)(lines)
replaceInputParamOfLines (n)(empty) = [[L(0,0)(IntTok 0)]]

replaceInputParam :: Int -> [L Token] -> [L Token]
replaceInputParam(n)(l:ls)
	| getToken(l) == (IdentTok "inputParam") = [L(0,0)(IdentTok varName)] ++ replaceInputParam(n)(ls)
	| otherwise = [l] ++ replaceInputParam(n)(ls)
	where varName = "var" ++ show(n)
replaceInputParam (n)(emptyList)  = [L(0,0)(Op_Plus)]
----------------done replacing the inputParam variable to var(n) 

--just stores the list of all the function names
namesOfFunctions = ["func1", "func2", "func3", "func4", "func5", "func6"]

----------------------------actually get rid of all the lines
{- this is how this is going to work, its going to keep replacing the value of the next variable with the pervious value of the variable
-}
--this function just removes the local variable decleration from the code
removeVariableDeclaration(line:lines) = if getToken(line!!0) == KW_Int || getToken(line!!0) == KW_Double then removeVariableDeclaration(lines) else [line] ++ removeVariableDeclaration(lines)
removeVariableDeclaration(emptyList) = []

--makes the entire function just a return statement
startParsingTheData (localVariables)(line:lines)
	| containsBool(line) = startParsingTheData(localVariables)(handleIfStatements(localVariables)(newLine)(lines))| lineContains(line)(Op_Equal) = startParsingTheData (newVars)(lines)
	| lineContains(line)(KW_Return) = drop 1 $ take (length newLine -1) newLine
	| otherwise = startParsingTheData(localVariables)(lines)---its a function declaration, brackets, class declaration, package
	where (l:ls) = line
	      newLine = [l] ++ replaceAllVariables(ls)(localVariables)
	      newVars = getNewVariables(newLine)(localVariables)
startParsingTheData (vars)(emptyList) = []
{-
the next few functions work together, they are an if else block and they evaluate the if statement and remove the section that will
never get called. These function also remove the if line and all the brackets associated with this block
-}
---------------------------------start of remove blocks
--is given the input, no barckets, no if or curly brackets, just converts it to True or False, it does this by converting it to a string first then a bool
handleIfStatements(localvariables)(l:ls)(lines) = deleteBlock(startConvertToBool2(l:ls))(lines)
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
---------------------------------end of remove blocks

------finds the close bracket the corresponds to this one
findCloseBracket(lines) = findCloseBracket2(lines)(0)
findCloseBracket2(line:lines)(n)
	| lineContains (line)(CloseCurly) && n == 0 = 1
	| lineContains (line)(CloseCurly) && n /= 0 = 1+ findCloseBracket2(lines)(n-1)
	| lineContains (line)(OpenCurly) = 1+findCloseBracket2(lines)(n+1)
	| otherwise = 1 + findCloseBracket2(lines)(n)
findCloseBracket2(emptyList)(n) = 0

--it finds the location of the boolean operator for example ==, >, <
locateBoolOperator (l:ls)
	| getToken(l) == Op_Equal = 0
	| getToken(l) == Op_GThan = 0
	| getToken(l) == Op_LThan = 0
	| otherwise = 1+locateBoolOperator (ls)

--------------------------these next functions just replace the expression into a boolean value
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
--------------------------end of making expressions into boolean values

----------------------------start of actually getting rid of all the lines
--l = variables, ls !!0 = '=', drop 1 ls = the value of the variable + ;
--takes the new variables and adjusts the variables list accordingly so the varaibles are updated
--adjusts the variables list to take into account the change that this line of code is doing to the variable
getNewVariables (l:ls)(variables) = take(pos)(variables) ++ [[l] ++ newVarValue] ++ drop(pos+1)(variables)
	where pos = isIn2DList(l)(variables)
	      newVarExpression = replaceAllVariables(ls)(variables)
	      newVarValue = removeSyntax(newVarExpression)


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

--check if the line given contains a certain variable
lineContainsVar (l:ls) = if isAnyVariable(getToken(l)) then True else lineContainsVar(ls)
lineContainsVar (emptyList) = False
----------------------------end of actually getting rid of all the lines
		
		
----------------------------start of get rid of blocks---------------------------------

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

--check returns a boolean representing if the input was a variable
--isAnyVariable (IntTok a) = True
--isAnyVariable (LongTok a) = True
--isAnyVariable (DoubleTok a) = True
--isAnyVariable (FloatTok a) = True
--isAnyVariable (CharTok a) = True
--isAnyVariable (StringTok a) = True
--isAnyVariable (BoolTok a) = True
isAnyVariable (IdentTok a) = True
isAnyVariable x = False

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
removeEmptyLoops (line:lines) = 
	if (lineContainsLoop (line)) then 
		 removeLoopIfEmpty (line:lines)
	else [line] ++ removeEmptyLoops (lines)
removeEmptyLoops emptyList = []

removeLoopIfEmpty (line:lines) = if (getToken((lines !! 0) !! 0) == CloseCurly) then removeEmptyLoops(drop 1 $ lines) else [line] ++ removeEmptyLoops(lines) 

--finds a open bracket
findOpenBrackets (l:ls) = if getToken(l) == OpenCurly then ls else findOpenBrackets (ls)
findOpenBrackets emptyList = emptyList
----done functions for remove emtpy loops----
		
--check if the line contains a loop
lineContainsLoop (l:ls) = if (getToken(l) == KW_If || getToken(l) == KW_Else || getToken(l) == KW_While || getToken(l) == KW_For) then True else lineContainsLoop (ls)
lineContainsLoop (emptyList) = False

 
--removes all the variables that are unsed, and removes them
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

--checks if the input is a variable
isVariable (IdentTok a) = True
isVariable x = False

--check if the line actually reasiigns the value of the variable
doesLineReassign(l:ls)(vars) =
	if isContainedInList (getToken(l)) (vars) then True
	else if getToken(l) == Op_Equal then False
	else doesLineReassign (ls)(vars)
doesLineReassign(emptyList)(vars) = False

--check if t is in the list
isContainedInList (t)(var:vars) =
	if t == var then 
		True 
	else
		isContainedInList(t)(vars)
isContainedInList (t)(emptyList) = False

--checks if the line is a statement
isLineAStatement (x:xs) = 
	if getToken(x) == Op_Equal then True else isLineAStatement(xs)
isLineAStatement (x) = False 

---check if the line contains a boolean value, or if it will even at compile time
containsBool(line) 
	| lineContains(line)(Op_Equals) = True 
	| lineContains(line)(Op_GThan) = True
	| lineContains(line)(Op_LThan) = True
	| otherwise = False

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

---------------converts the input to a list of lines, and the lines are represents as list of L Token
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
-------------end of making the input a list of lines

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
	if (lineContains(line)(IdentTok name) && ( lineContains(line)(KW_Private) ||  lineContains(line)(KW_Public)))then line:lines
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
-----------------end of find function functions


--used for getting arguments of the pos in L
getLineNumber (x, _) = x
getCharacterNumber(_, x) = x

--get the token, or position from the L data type
getToken (L _ y) = y
getPos (L y _) = y
					
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
	
getListValue (x:y:rst) = -getValue(y)
getListValue (x:xs) = getValue(x)
getListValue (emptyList) = 0
------------------------------------------------done functions that convert things back to normal values

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