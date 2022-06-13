functor PlcLexerFun(structure Tokens: PlcParser_TOKENS)=
   struct
    structure UserDeclarations =
      struct
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

end (* end of user routines *)
exception LexError (* raised if illegal leaf action tried *)
structure Internal =
	struct

datatype yyfinstate = N of int
type statedata = {fin : yyfinstate list, trans: string}
(* transition & final state table *)
val tab = let
val s = [ 
 (0, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000"
),
 (1, 
"\003\003\003\003\003\003\003\003\003\032\034\003\003\003\003\003\
\\003\003\003\003\003\003\003\003\003\003\003\003\003\003\003\003\
\\032\030\003\003\003\003\028\003\027\026\025\024\023\021\003\020\
\\018\018\018\018\018\018\018\018\018\018\016\015\013\011\003\003\
\\003\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\
\\007\007\007\007\007\007\007\007\007\007\007\010\003\009\003\007\
\\003\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\
\\007\007\007\007\007\007\007\007\007\007\007\006\005\004\003\003\
\\003"
),
 (7, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\008\008\008\008\008\008\008\008\008\008\000\000\000\000\000\000\
\\000\008\008\008\008\008\008\008\008\008\008\008\008\008\008\008\
\\008\008\008\008\008\008\008\008\008\008\008\000\000\000\000\008\
\\000\008\008\008\008\008\008\008\008\008\008\008\008\008\008\008\
\\008\008\008\008\008\008\008\008\008\008\008\000\000\000\000\000\
\\000"
),
 (11, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\012\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000"
),
 (13, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\014\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000"
),
 (16, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\017\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000"
),
 (18, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\019\019\019\019\019\019\019\019\019\019\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000"
),
 (21, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\022\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000"
),
 (28, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\029\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000"
),
 (30, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\031\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000"
),
 (32, 
"\000\000\000\000\000\000\000\000\000\033\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\033\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000"
),
(0, "")]
fun f x = x 
val s = List.map f (List.rev (tl (List.rev s))) 
exception LexHackingError 
fun look ((j,x)::r, i: int) = if i = j then x else look(r, i) 
  | look ([], i) = raise LexHackingError
fun g {fin=x, trans=i} = {fin=x, trans=look(s,i)} 
in Vector.fromList(List.map g 
[{fin = [], trans = 0},
{fin = [], trans = 1},
{fin = [], trans = 1},
{fin = [(N 64),(N 66)], trans = 0},
{fin = [(N 52),(N 64),(N 66)], trans = 0},
{fin = [(N 59),(N 64),(N 66)], trans = 0},
{fin = [(N 50),(N 64),(N 66)], trans = 0},
{fin = [(N 10),(N 64),(N 66)], trans = 7},
{fin = [(N 10)], trans = 7},
{fin = [(N 44),(N 64),(N 66)], trans = 0},
{fin = [(N 42),(N 64),(N 66)], trans = 0},
{fin = [(N 27),(N 64),(N 66)], trans = 11},
{fin = [(N 55)], trans = 0},
{fin = [(N 32),(N 64),(N 66)], trans = 13},
{fin = [(N 35)], trans = 0},
{fin = [(N 40),(N 64),(N 66)], trans = 0},
{fin = [(N 12),(N 64),(N 66)], trans = 16},
{fin = [(N 38)], trans = 0},
{fin = [(N 7),(N 64),(N 66)], trans = 18},
{fin = [(N 7)], trans = 18},
{fin = [(N 25),(N 64),(N 66)], trans = 0},
{fin = [(N 21),(N 64),(N 66)], trans = 21},
{fin = [(N 62)], trans = 0},
{fin = [(N 57),(N 64),(N 66)], trans = 0},
{fin = [(N 19),(N 64),(N 66)], trans = 0},
{fin = [(N 23),(N 64),(N 66)], trans = 0},
{fin = [(N 48),(N 64),(N 66)], trans = 0},
{fin = [(N 46),(N 64),(N 66)], trans = 0},
{fin = [(N 64),(N 66)], trans = 28},
{fin = [(N 17)], trans = 0},
{fin = [(N 14),(N 64),(N 66)], trans = 30},
{fin = [(N 30)], trans = 0},
{fin = [(N 4),(N 64),(N 66)], trans = 32},
{fin = [(N 4)], trans = 32},
{fin = [(N 1)], trans = 0}])
end
structure StartStates =
	struct
	datatype yystartstate = STARTSTATE of int

(* start state definitions *)

val INITIAL = STARTSTATE 1;

end
type result = UserDeclarations.lexresult
	exception LexerError (* raised if illegal leaf action tried *)
end

fun makeLexer yyinput =
let	val yygone0=1
	val yyb = ref "\n" 		(* buffer *)
	val yybl = ref 1		(*buffer length *)
	val yybufpos = ref 1		(* location of next character to use *)
	val yygone = ref yygone0	(* position in file of beginning of buffer *)
	val yydone = ref false		(* eof found yet? *)
	val yybegin = ref 1		(*Current 'start state' for lexer *)

	val YYBEGIN = fn (Internal.StartStates.STARTSTATE x) =>
		 yybegin := x

fun lex () : Internal.result =
let fun continue() = lex() in
  let fun scan (s,AcceptingLeaves : Internal.yyfinstate list list,l,i0) =
	let fun action (i,nil) = raise LexError
	| action (i,nil::l) = action (i-1,l)
	| action (i,(node::acts)::l) =
		case node of
		    Internal.N yyk => 
			(let fun yymktext() = String.substring(!yyb,i0,i-i0)
			     val yypos = i0+ !yygone
			open UserDeclarations Internal.StartStates
 in (yybufpos := i; case yyk of 

			(* Application actions *)

  1 => (lineNumber := !lineNumber + 1; lex())
| 10 => let val yytext=yymktext() in keyWord(yytext, yypos, yypos) end
| 12 => (COLON(yypos, yypos))
| 14 => (NOT(yypos, yypos))
| 17 => (AND(yypos, yypos))
| 19 => (PLUS(yypos, yypos))
| 21 => (MINUS(yypos, yypos))
| 23 => (TIMES(yypos, yypos))
| 25 => (DIV(yypos, yypos))
| 27 => (EQUAL(yypos, yypos))
| 30 => (DIFF(yypos, yypos))
| 32 => (LT(yypos, yypos))
| 35 => (LTE(yypos, yypos))
| 38 => (DCOL(yypos, yypos))
| 4 => (lex())
| 40 => (SEMICOL(yypos, yypos))
| 42 => (LSBR(yypos, yypos))
| 44 => (RSBR(yypos, yypos))
| 46 => (LPAR(yypos, yypos))
| 48 => (RPAR(yypos, yypos))
| 50 => (LBR(yypos, yypos))
| 52 => (RBR(yypos, yypos))
| 55 => (AFARROW(yypos, yypos))
| 57 => (COMMA(yypos, yypos))
| 59 => (PIPE(yypos, yypos))
| 62 => (MARROW(yypos, yypos))
| 64 => (lex())
| 66 => let val yytext=yymktext() in error("\n***Lexer error bad character ***\n"); raise Fail("Lexer error: bad character " ^yytext) end
| 7 => let val yytext=yymktext() in CINT(stoi(yytext), yypos, yypos) end
| _ => raise Internal.LexerError

		) end )

	val {fin,trans} = Unsafe.Vector.sub(Internal.tab, s)
	val NewAcceptingLeaves = fin::AcceptingLeaves
	in if l = !yybl then
	     if trans = #trans(Vector.sub(Internal.tab,0))
	       then action(l,NewAcceptingLeaves
) else	    let val newchars= if !yydone then "" else yyinput 1024
	    in if (String.size newchars)=0
		  then (yydone := true;
		        if (l=i0) then UserDeclarations.eof ()
		                  else action(l,NewAcceptingLeaves))
		  else (if i0=l then yyb := newchars
		     else yyb := String.substring(!yyb,i0,l-i0)^newchars;
		     yygone := !yygone+i0;
		     yybl := String.size (!yyb);
		     scan (s,AcceptingLeaves,l-i0,0))
	    end
	  else let val NewChar = Char.ord(Unsafe.CharVector.sub(!yyb,l))
		val NewChar = if NewChar<128 then NewChar else 128
		val NewState = Char.ord(Unsafe.CharVector.sub(trans,NewChar))
		in if NewState=0 then action(l,NewAcceptingLeaves)
		else scan(NewState,NewAcceptingLeaves,l+1,i0)
	end
	end
(*
	val start= if String.substring(!yyb,!yybufpos-1,1)="\n"
then !yybegin+1 else !yybegin
*)
	in scan(!yybegin (* start *),nil,!yybufpos,!yybufpos)
    end
end
  in lex
  end
end
