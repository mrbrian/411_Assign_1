{
module Main (main) where

import Data.Char (chr)
import System.Environment


}

%wrapper "monad"

$digit  = 0-9          -- digits
$alpha  = [a-zA-Z]       -- alphabetic characters
$newline = \n
 
tokens :-
	<0> $white+		  		{ skip }
	<0> "%" .* $newline	 		{ skip }
	<0> "if" 				{ getIf }
	<0> "then" 				{ getThen }
	<0> "while" 				{ getWhile }
	<0> "do" 				{ getDo }
	<0> "input" 				{ getInput }
	<0> "else" 				{ getElse }
	<0> "begin" 				{ getBegin }
	<0> "end" 				{ getEnd }
	<0> "write" 				{ getWrite }	
	<0> $digit+    				{ getNum }
	<0> $alpha [$alpha $digit]*	    	{ getId }	
	<0> "/*"                		{ nested_comment } 
	<0> "+"  				{ getAdd }
	<0> ":="  				{ getAssign }
	<0> "-"  				{ getSub }
	<0> "*"  				{ getMul }
	<0> "/"  				{ getDiv }
	<0> "("  				{ getLPar }
	<0> ")"					{ getRPar }
	<0> ";"					{ getSemi }
	
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

data Lexeme = IF AlexPosn	
			| THEN AlexPosn          
			| WHILE AlexPosn       
			| DO AlexPosn          
			| INPUT AlexPosn          
			| ELSE AlexPosn	
			| BEGIN AlexPosn          
			| END AlexPosn       
			| WRITE AlexPosn          
			| ID String AlexPosn          
			| NUM Int AlexPosn          
			| ADD AlexPosn          
			| ASSIGN AlexPosn       
			| SUB AlexPosn          
			| MUL AlexPosn          
			| DIV AlexPosn          
			| LPAR AlexPosn         
			| RPAR AlexPosn         
			| SEMICOLON AlexPosn  
			| LEOF
  deriving (Show,Eq)

getIf :: AlexInput -> Int -> Alex Lexeme
getIf (posn,c,_,inp) len =  return $ IF posn

getThen :: AlexInput -> Int -> Alex Lexeme
getThen (posn,c,_,inp) len =  return $ THEN posn

getWhile :: AlexInput -> Int -> Alex Lexeme
getWhile (posn,c,_,inp) len =  return $ WHILE posn

getDo :: AlexInput -> Int -> Alex Lexeme
getDo (posn,c,_,inp) len =  return $ DO posn

getInput :: AlexInput -> Int -> Alex Lexeme
getInput (posn,c,_,inp) len =  return $ INPUT posn

getElse :: AlexInput -> Int -> Alex Lexeme
getElse (posn,c,_,inp) len =  return $ ELSE posn

getBegin :: AlexInput -> Int -> Alex Lexeme
getBegin (posn,c,_,inp) len =  return $ BEGIN posn

getEnd :: AlexInput -> Int -> Alex Lexeme
getEnd (posn,c,_,inp) len =  return $ END posn

getWrite :: AlexInput -> Int -> Alex Lexeme
getWrite (posn,c,_,inp) len =  return $ WRITE posn

getId :: AlexInput -> Int -> Alex Lexeme
getId (posn,c,_,inp) len =  return $ ID (take len inp) posn

getNum :: AlexInput -> Int -> Alex Lexeme
getNum (posn,c,_,inp) len =  return $ NUM (read (take len inp)) posn

getAdd :: AlexInput -> Int -> Alex Lexeme
getAdd (posn,c,_,inp) len =  return $ ADD posn

getAssign :: AlexInput -> Int -> Alex Lexeme
getAssign (posn,c,_,inp) len =  return $ ASSIGN posn

getSub :: AlexInput -> Int -> Alex Lexeme
getSub (posn,c,_,inp) len =  return $ SUB posn

getMul :: AlexInput -> Int -> Alex Lexeme
getMul (posn,c,_,inp) len =  return $ MUL posn

getDiv :: AlexInput -> Int -> Alex Lexeme
getDiv (posn,c,_,inp) len =  return $ DIV posn

getLPar :: AlexInput -> Int -> Alex Lexeme
getLPar (posn,c,_,inp) len =  return $ LPAR posn

getRPar :: AlexInput -> Int -> Alex Lexeme
getRPar (posn,c,_,inp) len =  return $ RPAR posn

getSemi :: AlexInput -> Int -> Alex Lexeme
getSemi (posn,c,_,inp) len =  return $ SEMICOLON posn


alexEOF = return LEOF


tokens str = runAlex str $ do
               let loop = do tok <- alexMonadScan
                             if tok == LEOF
                               then return []
                               else do toks <- loop
                                       return $ tok : toks
               loop
          

nested_comment :: AlexInput -> Int -> Alex Lexeme
nested_comment _ _ = do
  input <- alexGetInput
  go 1 input
  where 
      go 0 input = do 
         alexSetInput input
         alexMonadScan
      go n input = do
              case alexGetByte input of
                 Nothing  -> err input
                 Just (c,input) -> do
                      case chr (fromIntegral c) of
                          '%' -> do
                              case alexGetByte input of
                                Nothing  -> err input
                                Just (c,input)  -> skip n input
                          '*' -> do
                              case alexGetByte input of
                                Nothing  -> err input
                                Just (47,input) -> go (n-1) input
                                Just (c,input)  -> go n input
                          '\47' -> do
                              case alexGetByte input of
                                Nothing  -> err input
                                Just (c,input) | c == fromIntegral (ord '*') -> go (n+1) input
                                Just (c,input)   -> go n input
                          c -> go n input
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

