{
module Main (main) where
import System.Environment
}

%wrapper "monadUserState"

$digit  = [0-9]          -- digits
$alpha  = [a-zA-Z]       -- alphabetic characters
$newline = \n
 
tokens :-

	<0, comment> "/*"			{ incLevel } 
	<comment> .					{ skip }
	<comment> "*/"				{ decLevel }
	<0>$white+               	; 
	<0>"%".*					;
	<0>newline               	;
--	<0>"/*" 					{ \p s -> comment s p 1 }
	<0>"if" 					{ \p s -> IF p }
	<0>"then" 					{ \p s -> THEN p }
	<0>"while" 					{ \p s -> WHILE p }
	<0>"do" 					{ \p s -> DO p }
	<0>"input" 					{ \p s -> INPUT p }
	<0>"else" 					{ \p s -> ELSE p }
	<0>"begin" 					{ \p s -> BEGIN p }
	<0>"end" 					{ \p s -> END p }
	<0>"write" 					{ \p s -> WRITE p }  
	<0>$alpha [$alpha $digit]*	{ \p s -> ID s p }  
	<0>$digit+ 					{ \p s -> NUM (read s) p }
	<0>"+"      				{ \p s -> ADD p }
	<0>":="     				{ \p s -> ASSIGN p }
	<0>"-"      				{ \p s -> SUB p }
	<0>"*"      				{ \p s -> MUL p }
	<0>"/"      				{ \p s -> DIV p }
	<0>"("      				{ \p s -> LPAR p }
	<0>")"      				{ \p s -> RPAR p }
	<0>";"      				{ \p s -> SEMICOLON p }

{
-- Each action has type :: String -> Token

-- The token type:
data Token =
	IF	AlexPosn            |  
	THEN AlexPosn			|   
	WHILE AlexPosn          |   
	DO AlexPosn             |   
	INPUT AlexPosn          |   
	ELSE AlexPosn           |   
	BEGIN AlexPosn          |   
	END AlexPosn            |   
	WRITE AlexPosn          |   
	ID String AlexPosn		|   
	NUM Int AlexPosn		|   
	ADD AlexPosn            |   
	ASSIGN AlexPosn         |   
	SUB AlexPosn            |   
	MUL AlexPosn            |   
	DIV AlexPosn            |   
	LPAR AlexPosn           |   
	RPAR AlexPosn           |   
	SEMICOLON AlexPosn      |
	Tint Int 				| 
	TkEOF 		
    deriving (Eq,Show)
			
incLevel :: AlexInput -> Int -> Alex  Token
incLevel a i
    = do pdepth <- alexGetUserState
         alexSetUserState (pdepth +1)
	 skip a i

decLevel :: AlexInput -> Int -> Alex   Token
decLevel a i
    = do pdepth <- alexGetUserState
         alexSetUserState (pdepth - 1)
	 skip a i

gettokens  str = runAlex str (loop []) where
    loop toks = do tok <- alexMonadScan;
                   case tok of
                        TkEOF  ->  return (reverse toks)
                        Tint j -> loop (j:toks)
					
-- Setting up the user states
type AlexUserState = Int        --  This is the type of the user state

alexInitUserState = 0          --  This is the initial configuation of the user state
	
-- An action that reports an error at the current position
lexError :: AlexInput -> Int -> Alex a
lexError (AlexPn _ l c, _, x:xs) _ = alexError $
    "lexical error at line " ++ show l ++ ", col " ++ show c ++ 
        ": unexpected " ++ show x

alexEOF :: Alex Token
alexEOF = return (TkEOF)

 }