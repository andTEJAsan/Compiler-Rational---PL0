(* pi.lex *)
structure T = Tokens
type pos = int
type svalue = T.svalue
type ('a,'b) token = ('a,'b) T.token
type lexresult = (svalue,pos) token
type lexarg = string
type arg = lexarg
val lin = ref 1;
val col = ref 0;
val eolpos = ref 0;
val badCh : string * string * int * int -> unit = fn
(fileName,bad,line,col) =>
TextIO.output(TextIO.stdOut,fileName^"["
^Int.toString line^"."^Int.toString col
^"] Invalid character \""^bad^"\"\n");
val error : string * int * int -> unit = fn
(e,l1,l2) => TextIO.output(TextIO.stdOut,"lex:line "
^Int.toString l1^" l2="^Int.toString l2
^": "^e^"\n");
val eof = fn fileName => (lin:=1;col:=0;T.TEOF (!lin,!col));
fun inc a = a:= !a +1;
%%
%header (functor PiLexFun(structure Tokens: Pi_TOKENS));
%arg (fileName:string);
alpha=[A-Za-z];
digit=[0-9];
ws = [\ \t];
eol = ("\013\010"|"\010"|"\013");
idchars = [A-Za-z'_0-9];
id= [A-Za-z]{idchars}*;
%%
{ws}* => (continue());
{eol} => (inc lin;eolpos:=yypos+size yytext;continue());
[~]?{digit}*"."{digit}*"("{digit}+")" => (T.TRATNUM((Rational.fromDecimal(yytext)),!lin,!col));
[~]?{digit}+ => (T.TINTNUM(BigInt.fromString(yytext),!lin,!col));
"tt" => (col:=yypos-(!eolpos);T.TBOOLNUM(true,!lin,!col));
"ff" => (col:=yypos-(!eolpos);T.TBOOLNUM(false,!lin,!col));
"if" => (col:=yypos-(!eolpos);T.TIF(!lin,!col));
"else" => (col:=yypos-(!eolpos);T.TELSE(!lin,!col));
"then" => (col:=yypos-(!eolpos);T.TTHEN(!lin,!col));
"fi" => (col:=yypos-(!eolpos);T.TFI(!lin,!col));
"while" => (col:=yypos-(!eolpos);T.TWHILE(!lin,!col));
"do" => (col:=yypos-(!eolpos);T.TDO(!lin,!col));
"od" => (col:=yypos-(!eolpos);T.TOD(!lin,!col));
"and" => (col:=yypos-(!eolpos);T.TAND(!lin,!col));
"or" => (col:=yypos-(!eolpos);T.TOR(!lin,!col));

"rational" => (col:=yypos-(!eolpos);T.TRATIONAL(!lin,!col));
"integer" => (col:=yypos-(!eolpos);T.TINTEGER(!lin,!col));
"boolean" => (col:=yypos-(!eolpos);T.TBOOLEAN(!lin,!col));
"print" => (col:=yypos-(!eolpos);T.TPRINT(!lin,!col));
";" => (col:=yypos-(!eolpos);T.TSEMI(!lin,!col));
"," => (col:=yypos-(!eolpos);T.TCOMMA(!lin,!col));
"(" => (col:=yypos-(!eolpos);T.TLPAREN(!lin,!col));
")" => (col:=yypos-(!eolpos);T.TRPAREN(!lin,!col));
"{" => (col:=yypos-(!eolpos);T.TLBRACE(!lin,!col));
"}" => (col:=yypos-(!eolpos);T.TRBRACE(!lin,!col));
":=" => (col:=yypos-(!eolpos);T.TASSIGN(!lin,!col));
"~" =>  (col:=yypos-(!eolpos);T.TNEG(!lin,!col));
"inverse" =>  (col:=yypos-(!eolpos);T.TINV(!lin,!col));
".+." =>   (col:=yypos-(!eolpos);T.TRATADD(!lin,!col));
".-." =>   (col:=yypos-(!eolpos);T.TRATSUB(!lin,!col));
".*." =>   (col:=yypos-(!eolpos);T.TRATMUL(!lin,!col));
"./." =>   (col:=yypos-(!eolpos);T.TRATDIV(!lin,!col));
"+" =>   (col:=yypos-(!eolpos);T.TADD(!lin,!col));
"-" =>   (col:=yypos-(!eolpos);T.TSUB(!lin,!col));
"*" =>   (col:=yypos-(!eolpos);T.TMUL(!lin,!col));
"/" =>   (col:=yypos-(!eolpos);T.TDIV(!lin,!col));
"%" =>   (col:=yypos-(!eolpos);T.TMOD(!lin,!col));
"&&" =>   (col:=yypos-(!eolpos);T.TAND(!lin,!col));
"||" =>   (col:=yypos-(!eolpos);T.TOR(!lin,!col));
"=" =>   (col:=yypos-(!eolpos);T.TEQ(!lin,!col));
"<>" =>   (col:=yypos-(!eolpos);T.TNE(!lin,!col));
"<" =>   (col:=yypos-(!eolpos);T.TLT(!lin,!col));
"<=" =>   (col:=yypos-(!eolpos);T.TLE(!lin,!col));
">" =>   (col:=yypos-(!eolpos);T.TGT(!lin,!col));
">=" =>   (col:=yypos-(!eolpos);T.TGE(!lin,!col));
"!" =>   (col:=yypos-(!eolpos);T.TNOT(!lin,!col));

{id} => (col:=yypos-(!eolpos);T.TIDEN(yytext,!lin,!col));
. => (print ("Unknown token found at " ^ (Int.toString (!lin)) ^ ": <" ^ yytext ^ ">. Continuing.\n"); continue());