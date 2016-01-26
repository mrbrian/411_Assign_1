
module A1 where

data Token = 
	| IF 
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
	

lexxer :: String -> [Token]
lexxer [] = []
lexxer (c:cx)
	| c == '+' = Tadd:(lexxer cx)	
	| c == '=' = Tassign:(lexxer cx)
	| c == '*' = Tmult:(lexxer cx)
	| otherwise = helper (c:cx)
	| "if" => IF
	| "then" => THEN
	| "while" => WHILE
	| "do" => DO
	| "input" => INPUT
	| "else" => ELSE
	| "begin" => BEGIN
	| "end" => END
	| "write" => WRITE
	| {alpha}[{digit}{alpha}]* => ID (identifier)
	| {digit}+ => NUM (positive integer)
	| "+" => ADD
	| ":=" => ASSIGN
	| "-" => SUB
	| "*" => MUL
	| "/" => DIV
	| "(" => LPAR
	| ")" => RPAR
	| ";"=> SEMICOLON
	
helper :: String -> [Token]
helper (x:xs) 
	| isLetter(x) = x == 'l' 



{-

how to get rid of left recursion?


prog -> stmt.
stmt -> IF expr THEN stmt ELSE stmt
            | WHILE expr DO stmt
            | INPUT ID
            | ID ASSIGN expr
            | WRITE expr
            | BEGIN stmtlist END.
stmtlist -> stmtlist stmt SEMICOLON
            |.
expr -> expr addop term
            | term.
addop -> ADD
            | SUB.
term -> term mulop factor
            | factor.
mulop -> MUL
            | DIV.
factor -> LPAR expr RPAR
            | ID
            | NUM
            | SUB NUM.

-}