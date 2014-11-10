bmiTell :: (RealFloat a) => a -> a -> String  
bmiTell weight height  
  | bmi <= skinny = "You're underweight, you emo, you!"  
  | bmi <= normal = "You're supposedly normal. Pffft, I bet you're ugly!"  
  | bmi <= fat    = "You're fat! Lose some weight, fatty!"  
  | otherwise     = "You're a whale, congratulations!"  
  where bmi = weight / height ^ 2 
        skinny = 18.5  
        normal = 25.0  
        fat = 30.0 
		
fillInValueForGlobalVariables (l:ls) globalVariables
	| ls == [] = []
	| isIn2DList(l)(globalVariables) == -1  = []
	| getToken(ls!!0) /= Op_Equal = [] --l:ls ++ [L (0,0) (IdentTok "2")]
	| otherwise  = take (isIn2DList(l)(globalVariables)) globalVariables ++ [globalVariables !! (isIn2DList(l)(globalVariables)) ++ take (length globalVariables - 2) drop 1 (globalVariables)] ++ drop (isIn2DList(l)(globalVariables)) globalVariables