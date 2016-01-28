module Main where

import Data.Char
import Data.List

data AlexPosn = AlexPn !Int !Int
        deriving (Eq,Show)

data Token = IF 		AlexPosn	
			| THEN 	    AlexPosn
			| WHILE     AlexPosn
			| DO        AlexPosn
			| INPUT     AlexPosn
			| ELSE      AlexPosn
			| BEGIN     AlexPosn
			| END       AlexPosn
			| WRITE     AlexPosn
			| ID String AlexPosn
			| NUM Int   AlexPosn
			| ADD       AlexPosn
			| ASSIGN    AlexPosn
			| SUB       AlexPosn
			| MUL       AlexPosn
			| DIV       AlexPosn
			| LPAR      AlexPosn
			| RPAR      AlexPosn
			| SEMICOLON AlexPosn
			deriving (Eq, Show)
	
comment :: String -> Int -> String
comment s n
	| n == 1 && isPrefixOf "*/" s = drop 2 s 
	| isPrefixOf "*/" s = comment (drop 2 s) (n-1) 
	| isPrefixOf "/*" s = comment (drop 2 s) (n+1) 
	| otherwise = comment (tail s) n		
		
lexxer :: AlexPosn -> String -> [Token]
lexxer _ [] = []
lexxer p s
	| isPrefixOf "/*" s = lexxer 	(AlexPn r (c+2)) (comment (drop 2 s) 1)
-- | isPrefixOf "*/" s = lexxer (drop 2 s)	--... error.
	| isPrefixOf "+" s 	= ADD 		(AlexPn r (c+1)) : (lexxer p xs)	
	| isPrefixOf "=" s 	= ASSIGN 	(AlexPn r (c+1)) : (lexxer p xs)	
	| isPrefixOf "-" s 	= SUB 		(AlexPn r (c+1)) : (lexxer p xs)	
	| isPrefixOf "*" s 	= MUL 		(AlexPn r (c+1)) : (lexxer p xs)	
	| isPrefixOf "/" s 	= DIV 		(AlexPn r (c+1)) : (lexxer p xs)	
	| isPrefixOf "(" s 	= LPAR 		(AlexPn r (c+1)) : (lexxer p xs)	
	| isPrefixOf ")" s 	= RPAR 		(AlexPn r (c+1)) : (lexxer p xs)	
	| isPrefixOf ";" s 	= SEMICOLON	(AlexPn r (c+1)) : (lexxer p xs)
	| otherwise 		= helper p s
	where 
		(AlexPn r c) = p
		x : xs = s
		
helper :: AlexPosn -> String -> [Token]
helper p s
	| isPrefixOf "if" 	s 	= IF  	p : (lexxer p rest)
	| isPrefixOf "then" s 	= THEN	p : (lexxer p rest)
	| isPrefixOf "while" s 	= WHILE p : (lexxer p rest)
	| isPrefixOf "do" s 	= DO 	p : (lexxer p rest)
	| isPrefixOf "input" s 	= INPUT p : (lexxer p rest)
	| isPrefixOf "else" s 	= ELSE	p : (lexxer p rest)
	| isPrefixOf "begin" s 	= BEGIN	p : (lexxer p rest)
	| isPrefixOf "end" s 	= END 	p : (lexxer p rest)
	| isPrefixOf "write" s 	= WRITE p : (lexxer p rest)
	| isDigit x 			= NUM (read num) p : (lexxer p rest2)
	| isSpace x 			= (lexxer p rest3)
	| otherwise 			= ID name p : (lexxer p rest)
	where 
		(AlexPn r c) = p
		x : xs = s
		(name, 	rest) 	= span (\x -> isAlpha x) s
		(num, 	rest2) 	= span (\x -> isDigit x) rest
		(_, 	rest3) 	= span (\x -> isSpace x) s
		