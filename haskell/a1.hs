module Main where

import Data.Char
import Data.List

data Token = IF 		
			| THEN 	    
			| WHILE     
			| DO        
			| INPUT     
			| ELSE      
			| BEGIN     
			| END       
			| WRITE     
			| ID String 
			| NUM Int   
			| ADD       
			| ASSIGN    
			| SUB       
			| MUL       
			| DIV       
			| LPAR      
			| RPAR      
			| SEMICOLON 
			deriving (Eq, Show)
	
comment :: String -> Int -> String
comment s n
	| n == 1 && isPrefixOf "*/" s = drop 2 s 
	| isPrefixOf "*/" s = comment (drop 2 s) (n-1) 
	| isPrefixOf "/*" s = comment (drop 2 s) (n+1) 
	| otherwise = comment (tail s) n		
		
lexxer :: String -> [Token]
lexxer [] = []
lexxer s
	| isPrefixOf "/*" s = lexxer (comment (drop 2 s) 1)
-- | isPrefixOf "*/" s = lexxer (drop 2 s)	--... error.
	| isPrefixOf "+" s 	= ADD 		: (lexxer xs)	
	| isPrefixOf "=" s 	= ASSIGN 	: (lexxer xs)	
	| isPrefixOf "-" s 	= SUB 		: (lexxer xs)	
	| isPrefixOf "*" s 	= MUL 		: (lexxer xs)	
	| isPrefixOf "/" s 	= DIV 		: (lexxer xs)	
	| isPrefixOf "(" s 	= LPAR 		: (lexxer xs)	
	| isPrefixOf ")" s 	= RPAR 		: (lexxer xs)	
	| isPrefixOf ";" s 	= SEMICOLON	: (lexxer xs)
	| otherwise 		= helper s
	where 
		x : xs = s
		
helper :: String -> [Token]
helper s
	| isPrefixOf "if" 	s 	= IF  	: (lexxer rest)
	| isPrefixOf "then" s 	= THEN	: (lexxer rest)
	| isPrefixOf "while" s 	= WHILE : (lexxer rest)
	| isPrefixOf "do" s 	= DO 	: (lexxer rest)
	| isPrefixOf "input" s 	= INPUT : (lexxer rest)
	| isPrefixOf "else" s 	= ELSE	: (lexxer rest)
	| isPrefixOf "begin" s 	= BEGIN	: (lexxer rest)
	| isPrefixOf "end" s 	= END 	: (lexxer rest)
	| isPrefixOf "write" s 	= WRITE : (lexxer rest)
	| isDigit x 			= NUM (read num) : (lexxer rest2)
	| isSpace x 			= (lexxer rest3)
	| otherwise 			= ID name : (lexxer rest)
	where 
		x : xs = s
		(name, 	rest) 	= span (\x -> isAlpha x) s
		(num, 	rest2) 	= span (\x -> isDigit x) rest
		(_, 	rest3) 	= span (\x -> isSpace x) s
		