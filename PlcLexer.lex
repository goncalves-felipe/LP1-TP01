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

fun keyWord(cmd, fpart, spart) =
    case cmd of 
        "var" => VAR(fpart, spart)
        | "fun" => FUN(fpart, spart)
        | "if" => IF(fpart, spart)
        | "then" => THEN(fpart, spart)
        | "else" => ELSE(fpart, spart)
        | "rec" => REC(fpart, spart)
        | "hd" => HD(fpart, spart)
        | "tl" => TL(fpart, spart)
        | "match" => MATCH(fpart, spart)
        | "with" => WITH(fpart, spart)
        | "ise" => ISE(fpart, spart)
        | "fn" => ANONF(fpart, spart)
        | "end" => END(fpart, spart)
        | "true" => TRUE(fpart, spart)
        | "false" => FALSE(fpart, spart)
        | "print" => PRINT(fpart, spart)
        | "Int" => INT(fpart, spart)
        | "Nil" => NIL(fpart, spart)
        | "Bool" => BOOL(fpart, spart)
        | "_" => UNDERSCORE(fpart, spart)
        | _   => NAME(cmd, fpart, spart)

fun stringToInt text =
    case Int.fromString text of
        SOME i => i
        | NONE => raise Fail("Error converting the string " ^ text)

(* Define what to do when the end of the file is reached. *)
fun eof() = Tokens.EOF(0,0)

(* Initialize the lexer. *)
fun init() =()

%%
%header(functor PlcLexerFun(structure Tokens: PlcParser_TOKENS));
alpha=[A-Za-z];
digit=[0-9];
whitespace=[\ \t];
identifier=[a-zA-Z_][a-zA-Z_0-9]*;
%s COMMENT;
startcomment=\(\*;
endcomment=\*\);

%%

\n => (lineNumber := !lineNumber + 1; lex());
{whitespace}+ => (lex());
{digit}+ => (CINT(stringToInt(yytext), yypos, yypos));
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
{startcomment} => (YYBEGIN COMMENT; lex());
{endcomment} => (YYBEGIN INITIAL; lex());
. => (lex());
. => (error("\n***Lexer error bad character ***\n"); raise Fail("Lexer error: bad character " ^yytext));