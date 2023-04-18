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
val eof = fn fileName => (lin:=1;col:=0;T.EOF (!lin,!col));
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
[~]?{digit}*"."{digit}*"("{digit}+")" => (T.RATNUM((Rational.fromDecimal(yytext)),!lin,!col));
[~]?{digit}+ => (T.INTNUM(BigInt.fromString(yytext),!lin,!col));
"tt" => (col:=yypos-(!eolpos);T.BOOLNUM(true,!lin,!col));
"ff" => (col:=yypos-(!eolpos);T.BOOLNUM(false,!lin,!col));
"if" => (col:=yypos-(!eolpos);T.IF(!lin,!col));
"else" => (col:=yypos-(!eolpos);T.ELSE(!lin,!col));
"then" => (col:=yypos-(!eolpos);T.THEN(!lin,!col));
"fi" => (col:=yypos-(!eolpos);T.FI(!lin,!col));
"while" => (col:=yypos-(!eolpos);T.WHILE(!lin,!col));
"do" => (col:=yypos-(!eolpos);T.DO(!lin,!col));
"od" => (col:=yypos-(!eolpos);T.OD(!lin,!col));

"rational" => (col:=yypos-(!eolpos);T.RATDL(!lin,!col));
"integer" => (col:=yypos-(!eolpos);T.INTDL(!lin,!col));
"boolean" => (col:=yypos-(!eolpos);T.BOOLDL(!lin,!col));
"print" => (col:=yypos-(!eolpos);T.PRINT(!lin,!col));
";" => (col:=yypos-(!eolpos);T.SEMI(!lin,!col));
"," => (col:=yypos-(!eolpos);T.COMMA(!lin,!col));
"(" => (col:=yypos-(!eolpos);T.LPAREN(!lin,!col));
")" => (col:=yypos-(!eolpos);T.RPAREN(!lin,!col));
"{" => (col:=yypos-(!eolpos);T.LBRACE(!lin,!col));
"}" => (col:=yypos-(!eolpos);T.RBRACE(!lin,!col));
":=" => (col:=yypos-(!eolpos);T.ASSIGN(!lin,!col));
"~" =>  (col:=yypos-(!eolpos);T.NEG(!lin,!col));
"inverse" =>  (col:=yypos-(!eolpos);T.INV(!lin,!col));
".+." =>   (col:=yypos-(!eolpos);T.RATPLUS(!lin,!col));
".-." =>   (col:=yypos-(!eolpos);T.RATSUB(!lin,!col));
".*." =>   (col:=yypos-(!eolpos);T.RATMUL(!lin,!col));
"./." =>   (col:=yypos-(!eolpos);T.RATDIV(!lin,!col));
"+" =>   (col:=yypos-(!eolpos);T.PLUS(!lin,!col));
"-" =>   (col:=yypos-(!eolpos);T.SUB(!lin,!col));
"*" =>   (col:=yypos-(!eolpos);T.MUL(!lin,!col));
"/" =>   (col:=yypos-(!eolpos);T.DIV(!lin,!col));
"%" =>   (col:=yypos-(!eolpos);T.MOD(!lin,!col));
"&&" =>   (col:=yypos-(!eolpos);T.AND(!lin,!col));
"||" =>   (col:=yypos-(!eolpos);T.OR(!lin,!col));
"=" =>   (col:=yypos-(!eolpos);T.EQ(!lin,!col));
"<>" =>   (col:=yypos-(!eolpos);T.NE(!lin,!col));
"<" =>   (col:=yypos-(!eolpos);T.LT(!lin,!col));
"<=" =>   (col:=yypos-(!eolpos);T.LE(!lin,!col));
">" =>   (col:=yypos-(!eolpos);T.GT(!lin,!col));
">=" =>   (col:=yypos-(!eolpos);T.GE(!lin,!col));
"!" =>   (col:=yypos-(!eolpos);T.NOT(!lin,!col));

{id} => (col:=yypos-(!eolpos);T.IDEN(yytext,!lin,!col));
. => (print ("Unknown token found at " ^ (Int.toString (!lin)) ^ ": <" ^ yytext ^ ">. Continuing.\n"); continue());