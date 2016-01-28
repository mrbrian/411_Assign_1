{
module Main (main) where
import System.Environment
}

%wrapper "posn"

$digit  = 0-9          -- digits
$alpha  = [a-zA-Z]       -- alphabetic characters
$newline = \n
 
tokens :-
	$white+               		; 
	newline               		;
	"/*" [. $newline]* "*/"		;
	"if" 					{ \p s -> IF p }
	"then" 					{ \p s -> THEN p }
	"while" 				{ \p s -> WHILE p }
	"do" 					{ \p s -> DO p }
	"input" 				{ \p s -> INPUT p }
	"else" 					{ \p s -> ELSE p }
	"begin" 				{ \p s -> BEGIN p }
	"end" 					{ \p s -> END p }
	"write" 				{ \p s -> WRITE p }  
	$alpha [$alpha $digit]*	{ \p s -> ID s p }  
	$digit+ 				{ \p s -> NUM (read s) p }
	"+"      				{ \p s -> ADD p }
	":="     				{ \p s -> ASSIGN p }
	"-"      				{ \p s -> SUB p }
	"*"      				{ \p s -> MUL p }
	"/"      				{ \p s -> DIV p }
	"("      				{ \p s -> LPAR p }
	")"      				{ \p s -> RPAR p }
	";"      				{ \p s -> SEMICOLON p }

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
	SEMICOLON AlexPosn      
    deriving (Eq,Show)
			

main = do 
    args <- getArgs
    case length args == 0 of
        True  -> do 
               let usage = "\nExpecting of the form < ./eng_lang inputfile > got < ./eng_lang >.\n\nTry again. :(\n"
               error $ "\n****************Error: Expecting file name as an argument." ++ usage
        False -> do
            let fname  = args !! 0 
            conts <- readFile fname
            let tokens = alexScanTokens conts 
            putStrLn "\n**************************************\n"
            putStrLn "The List of tokens are as follows.\n"
            mapM_ (putStrLn.show) tokens
}

