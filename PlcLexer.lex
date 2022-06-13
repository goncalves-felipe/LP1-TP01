(* Plc Lexer *)

(* User declarations *)

open Tokens
type pos = int
type slvalue = Tokens.svalue
type('a,'b) token =('a,'b) Tokens.token
type lexresult =(slvalue, pos)token

(* A function to print a message error on the screen. *)
val error = fn x => TextIO.output(TextIO.stdOut, x ^ "\n")
val lineNumber = ref 0

(* Get the current line being read. *)
fun getLineAsString() =
    let
        val lineNum = !lineNumber
    in
        Int.toString lineNum
    end

(* Define what to do when the end of the file is reached. *)
fun eof() = Tokens.EOF(0,0)

(* Initialize the lexer. *)
fun init() =()

fun keyWord(cmd, fpart, rpart) =
    case cmd of 
        "var" => VAR(fpart, rpart)
        | "fun" => FUN(fpart, rpart)
        | "if" => IF(fpart, rpart)
        | "then" => THEN(fpart, rpart)
        | "else" => ELSE(fpart, rpart)
        | "with" => WITH(fpart, rpart)
        | "match" => MATCH(fpart, rpart)
        | "rec" => REC(fpart, rpart)
        | "hd" => HD(fpart, rpart)
        | "print" => PRINT(fpart, rpart)
        | "end" => END(fpart, rpart)
        | "ise" => ISE(fpart, rpart)
        | "Bool" => BOOL(fpart, rpart)
        | "fn" => ANONF(fpart, rpart)
        | "tl" => TL(fpart, rpart)
        | "false" => FALSE(fpart, rpart)
        | "true" => TRUE(fpart, rpart)
        | "Nil" => NIL(fpart, rpart)
        | "Int" => INT(fpart, rpart)
        | "_" => UNDERSCORE(fpart, rpart)
        | _   => NAME(s, fpart, rpart)

fun stoi text =
    case Int.fromString text of
        SOME i => i
        | NONE => raise Fail("Erro ao converter a string " ^ text)

%%
%header(functor PlcLexerFun(structure Tokens: PlcParser_TOKENS));
alpha=[A-Za-z];
digit=[0-9];
whitespace=[\ \t];
identifier=[a-zA-Z_][a-zA-Z_0-9]*;

%%

\n => (lineNumber := !lineNumber + 1; lex());
{whitespace}+ => (lex());
{digit}+ => (CINT(stoi(yytext), yypos, yypos));
{identifier} => (keyWord(yytext, yypos, yypos));
":" => (COLON(yypos, yypos));
"!" => (NOT(yypos, yypos));
"&&" => (AND(yypos, yypos));
"+" => (PLUS(yypos, yypos));
"-" => (MINUS(yypos, yypos));
"*" => (TIMES(yypos, yypos));
"/" => (DIV(yypos, yypos));
"=" => (EQUAL(yypos, yypos));
"!=" => (DIFF(yypos, yypos));
"<" => (LT(yypos, yypos));
"<=" => (LTE(yypos, yypos));
"::" => (DCOL(yypos, yypos));
";" => (SEMICOL(yypos, yypos));
"[" => (LSBR(yypos, yypos));
"]" => (RSBR(yypos, yypos));
"(" => (LPAR(yypos, yypos));
")" => (RPAR(yypos, yypos));
"{" => (LBR(yypos, yypos));
"}" => (RBR(yypos, yypos));
"=>" => (AFARROW(yypos, yypos));
"," => (COMMA(yypos, yypos));
"|" => (PIPE(yypos, yypos));
"->" => (MARROW(yypos, yypos));
. => (lex());
. => (error("\n***Lexer error bad character ***\n"); raise Fail("Lexer error: bad character " ^yytext));