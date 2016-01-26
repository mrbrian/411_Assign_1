
module Main (main) where

import Data.Char

data Token = Tlet 
	| Tin
	| Tadd 
	| Tmult
	| Tassign
	| Num Int
	| Tvar String
	deriving (Eq, Show)
	
lexxer :: String -> [Token]
lexxer [] = []
lexxer (c:cx)
	| c == '+' 	= Tadd : (lexxer cx)	
	| c == '=' 	= Tassign : (lexxer cx)
	| c == '*' 	= Tmult : (lexxer cx)
	| otherwise = helper (c:cx)

helper :: String -> [Token]
helper s
	| s1 == "let" 	= Tlet : (lexxer rest)
	| s1 == "in" 	= Tin : (lexxer rest)		
	| isDigit x 	= Num (read num) : (lexxer rest2)
	| otherwise 	= Tvar s1 : (lexxer rest)
	where 
		x : xs = s
		(s1, rest) = span (\x -> isAlpha x) s
		(num, rest2) = span (\x -> isDigit x) rest
		
