/*
   This file checks that ambiguous tokens of the 
   same length are recognized in production order priority
   Thus "foo" matches the first and third rules
   Try > foobar "foo bar foobar tla" 
 */

%namespace LexScanner
%option noparser nofiles

alpha [a-zA-Z]
digit [0-9]

%%
if			Console.WriteLine("IF " + yytext);
then		Console.WriteLine("THEN " + yytext);
while		Console.WriteLine("WHILE" + yytext);
do			Console.WriteLine("DO " + yytext);
input		Console.WriteLine("INPUT" + yytext);
else		Console.WriteLine("ELSE " + yytext);
begin		Console.WriteLine("BEGIN " + yytext);
end	 		Console.WriteLine("END " + yytext);
write		Console.WriteLine("WRITE " + yytext);
{alpha}+	Console.WriteLine("ID " + yytext);
{digit}+	Console.WriteLine("NUM " + yytext);
+			Console.WriteLine("ADD " + yytext);
=			Console.WriteLine("ASSIGN " + yytext);
-			Console.WriteLine("SUB " + yytext);
*			Console.WriteLine("MUL " + yytext);
/			Console.WriteLine("DIV " + yytext);
\(			Console.WriteLine("LPAR " + yytext);
\)			Console.WriteLine("RPAR " + yytext);
;			Console.WriteLine("SEMICOLON " + yytext);
    


%%

    public static void Main(string[] argp) { 
        Scanner scnr = new Scanner();
        for (int i = 0; i < argp.Length; i++) {
            Console.WriteLine("Scanning \"" + argp[i] + "\"");
            scnr.SetSource(argp[i], 0);
            scnr.yylex();
        }
    }

