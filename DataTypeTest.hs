data Person = Person {	firstName :: String
					  , lastName :: String
					  , age :: Int
					  , height :: Float
					  , phoneNumber :: String
					  , flavor :: String
					  } deriving (Show,  Eq)

data Car = ALPHA | BETA | DELTA deriving (Show, Eq)

testFunc :: Car -> Bool
testFunc ALPHA = True
testFunc x = False