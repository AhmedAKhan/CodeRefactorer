import Language.Java.Lexer

combineTheLines (x:y:ys) = 
	if ( getLineNumber(getPos(x)) ==  getLineNumber(getPos(y))) then
		[x] ++ combineTheLines(y:ys)
	else
		[]
combineTheLines (x:y) = 
	if y == [] then 
		[]
	else if (getLineNumber(getPos(x)) == getLineNumber(getPos(y!!0))) then
		[x] ++ y
	else
		[] 
combineTheLines y = []

combineTheLines2 (lineNumber)(x:xs) = 
	if (getLineNumber(getPos(x)) == lineNumber) then
		[x] ++ combineTheLines2(lineNumber)(xs)
	else
		[] 
combineTheLines2 (lineNumber) (y) = []
	{-if (getLineNumber(getPos(y !! 0)) == lineNumber) then
		y
	else
		[] -}
combineTheLines(y) = combineTheLines2 (getLineNumber $ getPos $ (y!!0))(y)

getLineNumber (x, _) = x
getPos (L y _) = y