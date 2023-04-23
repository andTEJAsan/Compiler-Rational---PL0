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
%s PI COMMENT;
alpha=[A-Za-z];
digit=[0-9];
ws = [\ \t];
eol = ("\013\010"|"\010"|"\013");
idchars = [A-Za-z'_0-9];
id= [A-Za-z]{idchars}*;
%%
<INITIAL>{ws}* => (lin:=1; eolpos:=0; YYBEGIN PI; continue ());  
<PI>{ws}* => (continue());
<PI>{eol} => (inc lin;eolpos:=yypos+size yytext;continue());
<PI>[~]?{digit}*"."{digit}*"("{digit}+")" => (T.TRATNUM((Rational.fromDecimal(yytext)),!lin,!col));
<PI>[~]?{digit}+ => (T.TINTNUM(BigInt.fromString(yytext),!lin,!col));
<PI>"tt" => (col:=yypos-(!eolpos);T.TBOOLNUM(true,!lin,!col));
<PI>"ff" => (col:=yypos-(!eolpos);T.TBOOLNUM(false,!lin,!col));
<PI>"if" => (col:=yypos-(!eolpos);T.TIF(!lin,!col));
<PI>"else" => (col:=yypos-(!eolpos);T.TELSE(!lin,!col));
<PI>"then" => (col:=yypos-(!eolpos);T.TTHEN(!lin,!col));
<PI>"fi" => (col:=yypos-(!eolpos);T.TFI(!lin,!col));
<PI>"while" => (col:=yypos-(!eolpos);T.TWHILE(!lin,!col));
<PI>"do" => (col:=yypos-(!eolpos);T.TDO(!lin,!col));
<PI>"od" => (col:=yypos-(!eolpos);T.TOD(!lin,!col));
<PI>"and" => (col:=yypos-(!eolpos);T.TAND(!lin,!col));
<PI>"or" => (col:=yypos-(!eolpos);T.TOR(!lin,!col));
<PI>"procedure" => (col:=yypos-(!eolpos);T.TPROCEDURE(!lin,!col));
<PI>"read" => (col:=yypos-(!eolpos);T.TREAD(!lin,!col));
<PI>"call" => (col:=yypos-(!eolpos);T.TCALL(!lin,!col));
<PI>"make_rat" => (col:=yypos-(!eolpos);T.TMAKERAT(!lin,!col));
<PI>"fromDecimal" => (col:=yypos-(!eolpos);T.TFROMDECIMAL(!lin,!col));
<PI>"rational" => (col:=yypos-(!eolpos);T.TRATIONAL(!lin,!col));
<PI>"integer" => (col:=yypos-(!eolpos);T.TINTEGER(!lin,!col));
<PI>"boolean" => (col:=yypos-(!eolpos);T.TBOOLEAN(!lin,!col));
<PI>"print" => (col:=yypos-(!eolpos);T.TPRINT(!lin,!col));
<PI>";" => (col:=yypos-(!eolpos);T.TSEMI(!lin,!col));
<PI>"," => (col:=yypos-(!eolpos);T.TCOMMA(!lin,!col));
<PI>"(" => (col:=yypos-(!eolpos);T.TLPAREN(!lin,!col));
<PI>")" => (col:=yypos-(!eolpos);T.TRPAREN(!lin,!col));
<PI>"{" => (col:=yypos-(!eolpos);T.TLBRACE(!lin,!col));
<PI>"}" => (col:=yypos-(!eolpos);T.TRBRACE(!lin,!col));
<PI>":=" => (col:=yypos-(!eolpos);T.TASSIGN(!lin,!col));
<PI>"~" =>  (col:=yypos-(!eolpos);T.TNEG(!lin,!col));
<PI>"inverse" =>  (col:=yypos-(!eolpos);T.TINV(!lin,!col));
<PI>".+." =>   (col:=yypos-(!eolpos);T.TRATADD(!lin,!col));
<PI>".-." =>   (col:=yypos-(!eolpos);T.TRATSUB(!lin,!col));
<PI>".*." =>   (col:=yypos-(!eolpos);T.TRATMUL(!lin,!col));
<PI>"./." =>   (col:=yypos-(!eolpos);T.TRATDIV(!lin,!col));
<PI>"+" =>   (col:=yypos-(!eolpos);T.TADD(!lin,!col));
<PI>"-" =>   (col:=yypos-(!eolpos);T.TSUB(!lin,!col));
<PI>"*" =>   (col:=yypos-(!eolpos);T.TMUL(!lin,!col));
<PI>"/" =>   (col:=yypos-(!eolpos);T.TDIV(!lin,!col));
<PI>"%" =>   (col:=yypos-(!eolpos);T.TMOD(!lin,!col));
<PI>"&&" =>   (col:=yypos-(!eolpos);T.TAND(!lin,!col));
<PI>"||" =>   (col:=yypos-(!eolpos);T.TOR(!lin,!col));
<PI>"=" =>   (col:=yypos-(!eolpos);T.TEQ(!lin,!col));
<PI>"<>" =>   (col:=yypos-(!eolpos);T.TNE(!lin,!col));
<PI>"<" =>   (col:=yypos-(!eolpos);T.TLT(!lin,!col));
<PI>"<=" =>   (col:=yypos-(!eolpos);T.TLE(!lin,!col));
<PI>">" =>   (col:=yypos-(!eolpos);T.TGT(!lin,!col));
<PI>">=" =>   (col:=yypos-(!eolpos);T.TGE(!lin,!col));
<PI>"!" =>   (col:=yypos-(!eolpos);T.TNOT(!lin,!col));
<PI>{id} => (col:=yypos-(!eolpos);T.TIDEN(yytext,!lin,!col));
<PI>"(*" => (YYBEGIN COMMENT;continue());
<PI>. => (print ("Unknown token found at " ^ (Int.toString (!lin)) ^ ": <" ^ yytext ^ ">. Continuing.\n"); continue());
<COMMENT>"\n" => (lin:=(!lin)+1;eolpos:=yypos+size yytext; continue()  ) ;
<COMMENT>"*)" => (YYBEGIN PI; continue ()) ;
<COMMENT>. => (continue ());