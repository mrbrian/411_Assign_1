{-

-}


{
module Main (main) where

import Data.Char (chr)
import System.Environment

}

%wrapper "monad"

$digit  = 0-9          	-- digits
$alpha  = [a-zA-Z]      -- alphabetic characters
$newline = \n			
 
tokens :-
	<0> $white+		  				{ skip }
	<0> "%" .* $newline	 			{ skip }
	<0> "if" 						{ makeLexeme IF }
	<0> "then" 						{ makeLexeme THEN }
	<0> "while" 					{ makeLexeme WHILE }
	<0> "do" 						{ makeLexeme DO }
	<0> "input" 					{ makeLexeme INPUT }
	<0> "else" 						{ makeLexeme ELSE }
	<0> "begin" 					{ makeLexeme BEGIN }
	<0> "end" 						{ makeLexeme END }
	<0> "write" 					{ makeLexeme WRITE }	
	<0> $digit+    					{ \(p,c,b,s) len -> makeLexeme (NUM (read (take len s))) (p,c,b,s) len }
	<0> $alpha [$alpha $digit]*	    { \(p,c,b,s) len -> makeLexeme (ID (take len s)) (p,c,b,s) len }	
	<0> "/*"                		{ nested_comment } 
	<0> "+"  						{ makeLexeme ADD }
	<0> ":="  						{ makeLexeme ASSIGN }
	<0> "-"  						{ makeLexeme SUB }
	<0> "*"  						{ makeLexeme MUL }
	<0> "/"  						{ makeLexeme DIV }
	<0> "("  						{ makeLexeme LPAR }
	<0> ")"							{ makeLexeme RPAR }
	<0> ";"							{ makeLexeme SEMICOLON }
	
{

{-

type AlexInput =  (AlexPosn, Char, [Bytes], String)

data AlexState = AlexState {
        alex_pos :: !AlexPosn,  -- position at current input location
        alex_inp :: String,     -- the current input
        alex_chr :: !Char,      -- the character before the input
        alex_bytes :: [Byte],   -- rest of the bytes for the current char
        alex_scd :: !Int        -- the current startcode
    }

newtype Alex a = Alex { unAlex :: AlexState
                               -> Either String (AlexState, a) }

runAlex          :: String -> Alex a -> Either String a

alexGetInput     :: Alex AlexInput
alexSetInput     :: AlexInput -> Alex ()

alexError        :: String -> Alex a

alexGetStartCode :: Alex Int
alexSetStartCode :: Int -> Alex ()

alexMonadScan :: Alex result

The token actions should have the following type:

type AlexAction result = AlexInput -> Int -> Alex result
{ ... }  :: AlexAction result


 -}

data LexemeType = IF
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
  deriving (Show,Eq)
			
data Lexeme = L LexemeType AlexPosn	
			| LEOF
  deriving (Show,Eq)


{----------------------------------------------------------------------------------------------  
	makeLexeme - Creates a Lexeme instance, with a given LexemeType & AlexInput  
-----------------------------------------------------------------------------------------------}
makeLexeme :: LexemeType -> AlexInput -> Int -> Alex Lexeme
makeLexeme t (posn,c,_,inp) len =  return $ (L t posn)

alexEOF = return LEOF


tokens str = runAlex str $ do
               let loop = do tok <- alexMonadScan
                             if tok == LEOF
                               then return []
                               else do toks <- loop
                                       return $ tok : toks
               loop
          

{----------------------------------------------------------------------------------------------  
	nested_comment 	- Iterates through the input by chararacter, tracking the number of 
					- comment brackets exiting when enough closing brackets are found
-----------------------------------------------------------------------------------------------}
nested_comment :: AlexInput -> Int -> Alex Lexeme
nested_comment _ _ = do
  input <- alexGetInput
  go 1 input
  where 
      go 0 input = do 
         alexSetInput input
         alexMonadScan
      go n input = do
              case alexGetByte input of						-- get next byte(char) of input
                 Nothing  -> err input
                 Just (c,input) -> do
                      case chr (fromIntegral c) of
                          '%' -> do
                              case alexGetByte input of
                                Nothing  -> err input
                                Just (c,input)  -> skip n input
                          '*' -> do
                              case alexGetByte input of		-- get next byte(char) of input
                                Nothing  -> err input
                                Just (47,input) -> go (n-1) input
                                Just (c,input)  -> asterisk n input
                          '\47' -> do
                              case alexGetByte input of
                                Nothing  -> err input
                                Just (c,input) | c == fromIntegral (ord '*') -> go (n+1) input
                                Just (c,input)   -> go n input
                          c -> go n input                          
{----------------------------------------------------------------------------------------------  
	asterisk - Iterates through the input by chararacter, tracking the number of 
-----------------------------------------------------------------------------------------------}
      asterisk n input = do									-- last character was an asterisk, so check for a slash next.
              case alexGetByte input of
                 Nothing  -> err input
                 Just (c,input) -> do
                      case chr (fromIntegral c) of
                          '\47' -> go (n-1) input
                          c -> asterisk n input
{----------------------------------------------------------------------------------------------  
	skip - Iterates through the input by chararacter, tracking the number of 
-----------------------------------------------------------------------------------------------}
      skip n input = do
              case alexGetByte input of
                 Nothing  -> err input
                 Just (c,input) -> do
                      case chr (fromIntegral c) of
                          '\10' -> go n input
                          c -> skip n input
      err input = do 
        alexSetInput input;
        lexError $ "error in nested comment"


showPosn (AlexPn _ line col) = show line ++ ':': show col


lexError s = do
  (p,c,_,input) <- alexGetInput
  alexError (showPosn p ++ ": " ++ s ++ 
       (if (not (null input))
         then " before " ++ show (head input)
         else " at end of file"))
 

main = do 
    args <- getArgs
    case length args == 0 of
        True  -> do 
               let usage = "\nExpecting of the form < ./eng_lang inputfile > got < ./eng_lang >.\n\nTry again. :(\n"
               error $ "\n****************Error: Expecting file name as an argument." ++ usage
        False -> do
            let fname  = args !! 0 
            conts <- readFile fname
            let etok = tokens conts 
            case etok of
               Right tok -> do
                   putStrLn "\n**************************************\n"
                   putStrLn "The List of tokens are as follows.\n"
                   mapM_ (putStrLn.show) tok 
               Left msg -> do  
                  putStrLn msg    
                   

}

