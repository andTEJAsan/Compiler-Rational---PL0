functor PiLexFun(structure Tokens: Pi_TOKENS)  = struct

    structure yyInput : sig

        type stream
	val mkStream : (int -> string) -> stream
	val fromStream : TextIO.StreamIO.instream -> stream
	val getc : stream -> (Char.char * stream) option
	val getpos : stream -> int
	val getlineNo : stream -> int
	val subtract : stream * stream -> string
	val eof : stream -> bool
	val lastWasNL : stream -> bool

      end = struct

        structure TIO = TextIO
        structure TSIO = TIO.StreamIO
	structure TPIO = TextPrimIO

        datatype stream = Stream of {
            strm : TSIO.instream,
	    id : int,  (* track which streams originated 
			* from the same stream *)
	    pos : int,
	    lineNo : int,
	    lastWasNL : bool
          }

	local
	  val next = ref 0
	in
	fun nextId() = !next before (next := !next + 1)
	end

	val initPos = 2 (* ml-lex bug compatibility *)

	fun mkStream inputN = let
              val strm = TSIO.mkInstream 
			   (TPIO.RD {
			        name = "lexgen",
				chunkSize = 4096,
				readVec = SOME inputN,
				readArr = NONE,
				readVecNB = NONE,
				readArrNB = NONE,
				block = NONE,
				canInput = NONE,
				avail = (fn () => NONE),
				getPos = NONE,
				setPos = NONE,
				endPos = NONE,
				verifyPos = NONE,
				close = (fn () => ()),
				ioDesc = NONE
			      }, "")
	      in 
		Stream {strm = strm, id = nextId(), pos = initPos, lineNo = 1,
			lastWasNL = true}
	      end

	fun fromStream strm = Stream {
		strm = strm, id = nextId(), pos = initPos, lineNo = 1, lastWasNL = true
	      }

	fun getc (Stream {strm, pos, id, lineNo, ...}) = (case TSIO.input1 strm
              of NONE => NONE
	       | SOME (c, strm') => 
		   SOME (c, Stream {
			        strm = strm', 
				pos = pos+1, 
				id = id,
				lineNo = lineNo + 
					 (if c = #"\n" then 1 else 0),
				lastWasNL = (c = #"\n")
			      })
	     (* end case*))

	fun getpos (Stream {pos, ...}) = pos

	fun getlineNo (Stream {lineNo, ...}) = lineNo

	fun subtract (new, old) = let
	      val Stream {strm = strm, pos = oldPos, id = oldId, ...} = old
	      val Stream {pos = newPos, id = newId, ...} = new
              val (diff, _) = if newId = oldId andalso newPos >= oldPos
			      then TSIO.inputN (strm, newPos - oldPos)
			      else raise Fail 
				"BUG: yyInput: attempted to subtract incompatible streams"
	      in 
		diff 
	      end

	fun eof s = not (isSome (getc s))

	fun lastWasNL (Stream {lastWasNL, ...}) = lastWasNL

      end

    datatype yystart_state = 
PI | COMMENT | INITIAL
    structure UserDeclarations = 
      struct

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


      end

    datatype yymatch 
      = yyNO_MATCH
      | yyMATCH of yyInput.stream * action * yymatch
    withtype action = yyInput.stream * yymatch -> UserDeclarations.lexresult

    local

    val yytable = 
Vector.fromList []
    fun mk yyins = let
        (* current start state *)
        val yyss = ref INITIAL
	fun YYBEGIN ss = (yyss := ss)
	(* current input stream *)
        val yystrm = ref yyins
	(* get one char of input *)
	val yygetc = yyInput.getc
	(* create yytext *)
	fun yymktext(strm) = yyInput.subtract (strm, !yystrm)
        open UserDeclarations
        fun lex 
(yyarg as (fileName:string)) () = let 
     fun continue() = let
            val yylastwasn = yyInput.lastWasNL (!yystrm)
            fun yystuck (yyNO_MATCH) = raise Fail "stuck state"
	      | yystuck (yyMATCH (strm, action, old)) = 
		  action (strm, old)
	    val yypos = yyInput.getpos (!yystrm)
	    val yygetlineNo = yyInput.getlineNo
	    fun yyactsToMatches (strm, [],	  oldMatches) = oldMatches
	      | yyactsToMatches (strm, act::acts, oldMatches) = 
		  yyMATCH (strm, act, yyactsToMatches (strm, acts, oldMatches))
	    fun yygo actTable = 
		(fn (~1, _, oldMatches) => yystuck oldMatches
		  | (curState, strm, oldMatches) => let
		      val (transitions, finals') = Vector.sub (yytable, curState)
		      val finals = List.map (fn i => Vector.sub (actTable, i)) finals'
		      fun tryfinal() = 
		            yystuck (yyactsToMatches (strm, finals, oldMatches))
		      fun find (c, []) = NONE
			| find (c, (c1, c2, s)::ts) = 
		            if c1 <= c andalso c <= c2 then SOME s
			    else find (c, ts)
		      in case yygetc strm
			  of SOME(c, strm') => 
			       (case find (c, transitions)
				 of NONE => tryfinal()
				  | SOME n => 
				      yygo actTable
					(n, strm', 
					 yyactsToMatches (strm, finals, oldMatches)))
			   | NONE => tryfinal()
		      end)
	    in 
let
fun yyAction0 (strm, lastMatch : yymatch) = (yystrm := strm;
      (lin:=1; eolpos:=0; YYBEGIN PI; continue ()))
fun yyAction1 (strm, lastMatch : yymatch) = (yystrm := strm; (continue()))
fun yyAction2 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (inc lin;eolpos:=yypos+size yytext;continue())
      end
fun yyAction3 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (T.TRATNUM((Rational.fromDecimal(yytext)),!lin,!col))
      end
fun yyAction4 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (T.TINTNUM(BigInt.fromString(yytext),!lin,!col))
      end
fun yyAction5 (strm, lastMatch : yymatch) = (yystrm := strm;
      (col:=yypos-(!eolpos);T.TBOOLNUM(true,!lin,!col)))
fun yyAction6 (strm, lastMatch : yymatch) = (yystrm := strm;
      (col:=yypos-(!eolpos);T.TBOOLNUM(false,!lin,!col)))
fun yyAction7 (strm, lastMatch : yymatch) = (yystrm := strm;
      (col:=yypos-(!eolpos);T.TIF(!lin,!col)))
fun yyAction8 (strm, lastMatch : yymatch) = (yystrm := strm;
      (col:=yypos-(!eolpos);T.TELSE(!lin,!col)))
fun yyAction9 (strm, lastMatch : yymatch) = (yystrm := strm;
      (col:=yypos-(!eolpos);T.TTHEN(!lin,!col)))
fun yyAction10 (strm, lastMatch : yymatch) = (yystrm := strm;
      (col:=yypos-(!eolpos);T.TFI(!lin,!col)))
fun yyAction11 (strm, lastMatch : yymatch) = (yystrm := strm;
      (col:=yypos-(!eolpos);T.TRATUM(!lin,!col)))
fun yyAction12 (strm, lastMatch : yymatch) = (yystrm := strm;
      (col:=yypos-(!eolpos);T.TWHILE(!lin,!col)))
fun yyAction13 (strm, lastMatch : yymatch) = (yystrm := strm;
      (col:=yypos-(!eolpos);T.TDO(!lin,!col)))
fun yyAction14 (strm, lastMatch : yymatch) = (yystrm := strm;
      (col:=yypos-(!eolpos);T.TOD(!lin,!col)))
fun yyAction15 (strm, lastMatch : yymatch) = (yystrm := strm;
      (col:=yypos-(!eolpos);T.TAND(!lin,!col)))
fun yyAction16 (strm, lastMatch : yymatch) = (yystrm := strm;
      (col:=yypos-(!eolpos);T.TOR(!lin,!col)))
fun yyAction17 (strm, lastMatch : yymatch) = (yystrm := strm;
      (col:=yypos-(!eolpos);T.TPROCEDURE(!lin,!col)))
fun yyAction18 (strm, lastMatch : yymatch) = (yystrm := strm;
      (col:=yypos-(!eolpos);T.TREAD(!lin,!col)))
fun yyAction19 (strm, lastMatch : yymatch) = (yystrm := strm;
      (col:=yypos-(!eolpos);T.TCALL(!lin,!col)))
fun yyAction20 (strm, lastMatch : yymatch) = (yystrm := strm;
      (col:=yypos-(!eolpos);T.TMAKERAT(!lin,!col)))
fun yyAction21 (strm, lastMatch : yymatch) = (yystrm := strm;
      (col:=yypos-(!eolpos);T.TFROMDECIMAL(!lin,!col)))
fun yyAction22 (strm, lastMatch : yymatch) = (yystrm := strm;
      (col:=yypos-(!eolpos);T.TRATIONAL(!lin,!col)))
fun yyAction23 (strm, lastMatch : yymatch) = (yystrm := strm;
      (col:=yypos-(!eolpos);T.TINTEGER(!lin,!col)))
fun yyAction24 (strm, lastMatch : yymatch) = (yystrm := strm;
      (col:=yypos-(!eolpos);T.TBOOLEAN(!lin,!col)))
fun yyAction25 (strm, lastMatch : yymatch) = (yystrm := strm;
      (col:=yypos-(!eolpos);T.TPRINT(!lin,!col)))
fun yyAction26 (strm, lastMatch : yymatch) = (yystrm := strm;
      (col:=yypos-(!eolpos);T.TSEMI(!lin,!col)))
fun yyAction27 (strm, lastMatch : yymatch) = (yystrm := strm;
      (col:=yypos-(!eolpos);T.TCOMMA(!lin,!col)))
fun yyAction28 (strm, lastMatch : yymatch) = (yystrm := strm;
      (col:=yypos-(!eolpos);T.TLPAREN(!lin,!col)))
fun yyAction29 (strm, lastMatch : yymatch) = (yystrm := strm;
      (col:=yypos-(!eolpos);T.TRPAREN(!lin,!col)))
fun yyAction30 (strm, lastMatch : yymatch) = (yystrm := strm;
      (col:=yypos-(!eolpos);T.TLBRACE(!lin,!col)))
fun yyAction31 (strm, lastMatch : yymatch) = (yystrm := strm;
      (col:=yypos-(!eolpos);T.TRBRACE(!lin,!col)))
fun yyAction32 (strm, lastMatch : yymatch) = (yystrm := strm;
      (col:=yypos-(!eolpos);T.TASSIGN(!lin,!col)))
fun yyAction33 (strm, lastMatch : yymatch) = (yystrm := strm;
      (col:=yypos-(!eolpos);T.TNEG(!lin,!col)))
fun yyAction34 (strm, lastMatch : yymatch) = (yystrm := strm;
      (col:=yypos-(!eolpos);T.TINV(!lin,!col)))
fun yyAction35 (strm, lastMatch : yymatch) = (yystrm := strm;
      (col:=yypos-(!eolpos);T.TRATADD(!lin,!col)))
fun yyAction36 (strm, lastMatch : yymatch) = (yystrm := strm;
      (col:=yypos-(!eolpos);T.TRATSUB(!lin,!col)))
fun yyAction37 (strm, lastMatch : yymatch) = (yystrm := strm;
      (col:=yypos-(!eolpos);T.TRATMUL(!lin,!col)))
fun yyAction38 (strm, lastMatch : yymatch) = (yystrm := strm;
      (col:=yypos-(!eolpos);T.TRATDIV(!lin,!col)))
fun yyAction39 (strm, lastMatch : yymatch) = (yystrm := strm;
      (col:=yypos-(!eolpos);T.TADD(!lin,!col)))
fun yyAction40 (strm, lastMatch : yymatch) = (yystrm := strm;
      (col:=yypos-(!eolpos);T.TSUB(!lin,!col)))
fun yyAction41 (strm, lastMatch : yymatch) = (yystrm := strm;
      (col:=yypos-(!eolpos);T.TMUL(!lin,!col)))
fun yyAction42 (strm, lastMatch : yymatch) = (yystrm := strm;
      (col:=yypos-(!eolpos);T.TDIV(!lin,!col)))
fun yyAction43 (strm, lastMatch : yymatch) = (yystrm := strm;
      (col:=yypos-(!eolpos);T.TMOD(!lin,!col)))
fun yyAction44 (strm, lastMatch : yymatch) = (yystrm := strm;
      (col:=yypos-(!eolpos);T.TAND(!lin,!col)))
fun yyAction45 (strm, lastMatch : yymatch) = (yystrm := strm;
      (col:=yypos-(!eolpos);T.TOR(!lin,!col)))
fun yyAction46 (strm, lastMatch : yymatch) = (yystrm := strm;
      (col:=yypos-(!eolpos);T.TEQ(!lin,!col)))
fun yyAction47 (strm, lastMatch : yymatch) = (yystrm := strm;
      (col:=yypos-(!eolpos);T.TNE(!lin,!col)))
fun yyAction48 (strm, lastMatch : yymatch) = (yystrm := strm;
      (col:=yypos-(!eolpos);T.TLT(!lin,!col)))
fun yyAction49 (strm, lastMatch : yymatch) = (yystrm := strm;
      (col:=yypos-(!eolpos);T.TLE(!lin,!col)))
fun yyAction50 (strm, lastMatch : yymatch) = (yystrm := strm;
      (col:=yypos-(!eolpos);T.TGT(!lin,!col)))
fun yyAction51 (strm, lastMatch : yymatch) = (yystrm := strm;
      (col:=yypos-(!eolpos);T.TGE(!lin,!col)))
fun yyAction52 (strm, lastMatch : yymatch) = (yystrm := strm;
      (col:=yypos-(!eolpos);T.TNOT(!lin,!col)))
fun yyAction53 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (col:=yypos-(!eolpos);T.TIDEN(yytext,!lin,!col))
      end
fun yyAction54 (strm, lastMatch : yymatch) = (yystrm := strm;
      (YYBEGIN COMMENT;continue()))
fun yyAction55 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
        (print ("Unknown token found at " ^ (Int.toString (!lin)) ^ ": <" ^ yytext ^ ">. Continuing.\n"); continue())
      end
fun yyAction56 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (lin:=(!lin)+1;eolpos:=yypos+size yytext; continue()  )
      end
fun yyAction57 (strm, lastMatch : yymatch) = (yystrm := strm;
      (YYBEGIN PI; continue ()))
fun yyAction58 (strm, lastMatch : yymatch) = (yystrm := strm; (continue ()))
fun yyQ2 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE =>
            if yyInput.eof(!(yystrm))
              then UserDeclarations.eof(yyarg)
              else yyAction0(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"\n"
              then if yyInput.eof(!(yystrm))
                  then UserDeclarations.eof(yyarg)
                  else yyAction0(strm, yyNO_MATCH)
            else if inp < #"\n"
              then if inp = #"\t"
                  then yyQ2(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
                else if yyInput.eof(!(yystrm))
                  then UserDeclarations.eof(yyarg)
                  else yyAction0(strm, yyNO_MATCH)
            else if inp = #" "
              then yyQ2(strm', yyMATCH(strm, yyAction0, yyNO_MATCH))
            else if yyInput.eof(!(yystrm))
              then UserDeclarations.eof(yyarg)
              else yyAction0(strm, yyNO_MATCH)
      (* end case *))
fun yyQ144 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction57(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction57(strm, yyNO_MATCH)
      (* end case *))
fun yyQ143 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction58(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #")"
              then yyQ144(strm', yyMATCH(strm, yyAction58, yyNO_MATCH))
              else yyAction58(strm, yyNO_MATCH)
      (* end case *))
fun yyQ142 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction56(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction56(strm, yyNO_MATCH)
      (* end case *))
fun yyQ141 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction58(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction58(strm, yyNO_MATCH)
      (* end case *))
fun yyQ1 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE =>
            if yyInput.eof(!(yystrm))
              then UserDeclarations.eof(yyarg)
              else yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"\v"
              then yyQ141(strm', lastMatch)
            else if inp < #"\v"
              then if inp = #"\n"
                  then yyQ142(strm', lastMatch)
                  else yyQ141(strm', lastMatch)
            else if inp = #"*"
              then yyQ143(strm', lastMatch)
              else yyQ141(strm', lastMatch)
      (* end case *))
fun yyQ46 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction3(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction3(strm, yyNO_MATCH)
      (* end case *))
fun yyQ45 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"*"
              then yystuck(lastMatch)
            else if inp < #"*"
              then if inp = #")"
                  then yyQ46(strm', lastMatch)
                  else yystuck(lastMatch)
            else if inp = #"0"
              then yyQ45(strm', lastMatch)
            else if inp < #"0"
              then yystuck(lastMatch)
            else if inp <= #"9"
              then yyQ45(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ44 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"0"
              then yyQ45(strm', lastMatch)
            else if inp < #"0"
              then yystuck(lastMatch)
            else if inp <= #"9"
              then yyQ45(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ42 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #")"
              then yystuck(lastMatch)
            else if inp < #")"
              then if inp = #"("
                  then yyQ44(strm', lastMatch)
                  else yystuck(lastMatch)
            else if inp = #"0"
              then yyQ42(strm', lastMatch)
            else if inp < #"0"
              then yystuck(lastMatch)
            else if inp <= #"9"
              then yyQ42(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ43 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction4(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"/"
              then yyAction4(strm, yyNO_MATCH)
            else if inp < #"/"
              then if inp = #"."
                  then yyQ42(strm', yyMATCH(strm, yyAction4, yyNO_MATCH))
                  else yyAction4(strm, yyNO_MATCH)
            else if inp <= #"9"
              then yyQ43(strm', yyMATCH(strm, yyAction4, yyNO_MATCH))
              else yyAction4(strm, yyNO_MATCH)
      (* end case *))
fun yyQ41 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction33(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"/"
              then yyAction33(strm, yyNO_MATCH)
            else if inp < #"/"
              then if inp = #"."
                  then yyQ42(strm', yyMATCH(strm, yyAction33, yyNO_MATCH))
                  else yyAction33(strm, yyNO_MATCH)
            else if inp <= #"9"
              then yyQ43(strm', yyMATCH(strm, yyAction33, yyNO_MATCH))
              else yyAction33(strm, yyNO_MATCH)
      (* end case *))
fun yyQ40 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction31(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction31(strm, yyNO_MATCH)
      (* end case *))
fun yyQ47 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction45(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction45(strm, yyNO_MATCH)
      (* end case *))
fun yyQ39 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction55(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"|"
              then yyQ47(strm', yyMATCH(strm, yyAction55, yyNO_MATCH))
              else yyAction55(strm, yyNO_MATCH)
      (* end case *))
fun yyQ38 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction30(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction30(strm, yyNO_MATCH)
      (* end case *))
fun yyQ48 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction53(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"A"
              then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
            else if inp < #"A"
              then if inp = #"("
                  then yyAction53(strm, yyNO_MATCH)
                else if inp < #"("
                  then if inp = #"'"
                      then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                      else yyAction53(strm, yyNO_MATCH)
                else if inp = #"0"
                  then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                else if inp < #"0"
                  then yyAction53(strm, yyNO_MATCH)
                else if inp <= #"9"
                  then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                  else yyAction53(strm, yyNO_MATCH)
            else if inp = #"`"
              then yyAction53(strm, yyNO_MATCH)
            else if inp < #"`"
              then if inp = #"["
                  then yyAction53(strm, yyNO_MATCH)
                else if inp < #"["
                  then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                else if inp = #"_"
                  then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                  else yyAction53(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
              else yyAction53(strm, yyNO_MATCH)
      (* end case *))
fun yyQ52 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction12(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"A"
              then yyQ48(strm', yyMATCH(strm, yyAction12, yyNO_MATCH))
            else if inp < #"A"
              then if inp = #"("
                  then yyAction12(strm, yyNO_MATCH)
                else if inp < #"("
                  then if inp = #"'"
                      then yyQ48(strm', yyMATCH(strm, yyAction12, yyNO_MATCH))
                      else yyAction12(strm, yyNO_MATCH)
                else if inp = #"0"
                  then yyQ48(strm', yyMATCH(strm, yyAction12, yyNO_MATCH))
                else if inp < #"0"
                  then yyAction12(strm, yyNO_MATCH)
                else if inp <= #"9"
                  then yyQ48(strm', yyMATCH(strm, yyAction12, yyNO_MATCH))
                  else yyAction12(strm, yyNO_MATCH)
            else if inp = #"`"
              then yyAction12(strm, yyNO_MATCH)
            else if inp < #"`"
              then if inp = #"["
                  then yyAction12(strm, yyNO_MATCH)
                else if inp < #"["
                  then yyQ48(strm', yyMATCH(strm, yyAction12, yyNO_MATCH))
                else if inp = #"_"
                  then yyQ48(strm', yyMATCH(strm, yyAction12, yyNO_MATCH))
                  else yyAction12(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ48(strm', yyMATCH(strm, yyAction12, yyNO_MATCH))
              else yyAction12(strm, yyNO_MATCH)
      (* end case *))
fun yyQ51 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction53(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction53(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #"0"
                  then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                else if inp < #"0"
                  then if inp = #"'"
                      then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                      else yyAction53(strm, yyNO_MATCH)
                else if inp = #":"
                  then yyAction53(strm, yyNO_MATCH)
                else if inp < #":"
                  then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction53(strm, yyNO_MATCH)
                  else yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
            else if inp = #"a"
              then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
            else if inp < #"a"
              then if inp = #"_"
                  then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                  else yyAction53(strm, yyNO_MATCH)
            else if inp = #"f"
              then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
            else if inp < #"f"
              then if inp = #"e"
                  then yyQ52(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                  else yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
              else yyAction53(strm, yyNO_MATCH)
      (* end case *))
fun yyQ50 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction53(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction53(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #"0"
                  then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                else if inp < #"0"
                  then if inp = #"'"
                      then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                      else yyAction53(strm, yyNO_MATCH)
                else if inp = #":"
                  then yyAction53(strm, yyNO_MATCH)
                else if inp < #":"
                  then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction53(strm, yyNO_MATCH)
                  else yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
            else if inp = #"a"
              then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
            else if inp < #"a"
              then if inp = #"_"
                  then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                  else yyAction53(strm, yyNO_MATCH)
            else if inp = #"m"
              then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
            else if inp < #"m"
              then if inp = #"l"
                  then yyQ51(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                  else yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
              else yyAction53(strm, yyNO_MATCH)
      (* end case *))
fun yyQ49 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction53(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction53(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #"0"
                  then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                else if inp < #"0"
                  then if inp = #"'"
                      then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                      else yyAction53(strm, yyNO_MATCH)
                else if inp = #":"
                  then yyAction53(strm, yyNO_MATCH)
                else if inp < #":"
                  then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction53(strm, yyNO_MATCH)
                  else yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
            else if inp = #"a"
              then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
            else if inp < #"a"
              then if inp = #"_"
                  then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                  else yyAction53(strm, yyNO_MATCH)
            else if inp = #"j"
              then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
            else if inp < #"j"
              then if inp = #"i"
                  then yyQ50(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                  else yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
              else yyAction53(strm, yyNO_MATCH)
      (* end case *))
fun yyQ37 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction53(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction53(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #"0"
                  then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                else if inp < #"0"
                  then if inp = #"'"
                      then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                      else yyAction53(strm, yyNO_MATCH)
                else if inp = #":"
                  then yyAction53(strm, yyNO_MATCH)
                else if inp < #":"
                  then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction53(strm, yyNO_MATCH)
                  else yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
            else if inp = #"a"
              then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
            else if inp < #"a"
              then if inp = #"_"
                  then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                  else yyAction53(strm, yyNO_MATCH)
            else if inp = #"i"
              then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
            else if inp < #"i"
              then if inp = #"h"
                  then yyQ49(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                  else yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
              else yyAction53(strm, yyNO_MATCH)
      (* end case *))
fun yyQ54 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction5(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"A"
              then yyQ48(strm', yyMATCH(strm, yyAction5, yyNO_MATCH))
            else if inp < #"A"
              then if inp = #"("
                  then yyAction5(strm, yyNO_MATCH)
                else if inp < #"("
                  then if inp = #"'"
                      then yyQ48(strm', yyMATCH(strm, yyAction5, yyNO_MATCH))
                      else yyAction5(strm, yyNO_MATCH)
                else if inp = #"0"
                  then yyQ48(strm', yyMATCH(strm, yyAction5, yyNO_MATCH))
                else if inp < #"0"
                  then yyAction5(strm, yyNO_MATCH)
                else if inp <= #"9"
                  then yyQ48(strm', yyMATCH(strm, yyAction5, yyNO_MATCH))
                  else yyAction5(strm, yyNO_MATCH)
            else if inp = #"`"
              then yyAction5(strm, yyNO_MATCH)
            else if inp < #"`"
              then if inp = #"["
                  then yyAction5(strm, yyNO_MATCH)
                else if inp < #"["
                  then yyQ48(strm', yyMATCH(strm, yyAction5, yyNO_MATCH))
                else if inp = #"_"
                  then yyQ48(strm', yyMATCH(strm, yyAction5, yyNO_MATCH))
                  else yyAction5(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ48(strm', yyMATCH(strm, yyAction5, yyNO_MATCH))
              else yyAction5(strm, yyNO_MATCH)
      (* end case *))
fun yyQ56 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction9(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"A"
              then yyQ48(strm', yyMATCH(strm, yyAction9, yyNO_MATCH))
            else if inp < #"A"
              then if inp = #"("
                  then yyAction9(strm, yyNO_MATCH)
                else if inp < #"("
                  then if inp = #"'"
                      then yyQ48(strm', yyMATCH(strm, yyAction9, yyNO_MATCH))
                      else yyAction9(strm, yyNO_MATCH)
                else if inp = #"0"
                  then yyQ48(strm', yyMATCH(strm, yyAction9, yyNO_MATCH))
                else if inp < #"0"
                  then yyAction9(strm, yyNO_MATCH)
                else if inp <= #"9"
                  then yyQ48(strm', yyMATCH(strm, yyAction9, yyNO_MATCH))
                  else yyAction9(strm, yyNO_MATCH)
            else if inp = #"`"
              then yyAction9(strm, yyNO_MATCH)
            else if inp < #"`"
              then if inp = #"["
                  then yyAction9(strm, yyNO_MATCH)
                else if inp < #"["
                  then yyQ48(strm', yyMATCH(strm, yyAction9, yyNO_MATCH))
                else if inp = #"_"
                  then yyQ48(strm', yyMATCH(strm, yyAction9, yyNO_MATCH))
                  else yyAction9(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ48(strm', yyMATCH(strm, yyAction9, yyNO_MATCH))
              else yyAction9(strm, yyNO_MATCH)
      (* end case *))
fun yyQ55 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction53(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction53(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #"0"
                  then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                else if inp < #"0"
                  then if inp = #"'"
                      then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                      else yyAction53(strm, yyNO_MATCH)
                else if inp = #":"
                  then yyAction53(strm, yyNO_MATCH)
                else if inp < #":"
                  then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction53(strm, yyNO_MATCH)
                  else yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
            else if inp = #"a"
              then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
            else if inp < #"a"
              then if inp = #"_"
                  then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                  else yyAction53(strm, yyNO_MATCH)
            else if inp = #"o"
              then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
            else if inp < #"o"
              then if inp = #"n"
                  then yyQ56(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                  else yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
              else yyAction53(strm, yyNO_MATCH)
      (* end case *))
fun yyQ53 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction53(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction53(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #"0"
                  then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                else if inp < #"0"
                  then if inp = #"'"
                      then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                      else yyAction53(strm, yyNO_MATCH)
                else if inp = #":"
                  then yyAction53(strm, yyNO_MATCH)
                else if inp < #":"
                  then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction53(strm, yyNO_MATCH)
                  else yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
            else if inp = #"a"
              then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
            else if inp < #"a"
              then if inp = #"_"
                  then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                  else yyAction53(strm, yyNO_MATCH)
            else if inp = #"f"
              then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
            else if inp < #"f"
              then if inp = #"e"
                  then yyQ55(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                  else yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
              else yyAction53(strm, yyNO_MATCH)
      (* end case *))
fun yyQ36 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction53(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"_"
              then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
            else if inp < #"_"
              then if inp = #"0"
                  then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                else if inp < #"0"
                  then if inp = #"'"
                      then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                      else yyAction53(strm, yyNO_MATCH)
                else if inp = #"A"
                  then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                else if inp < #"A"
                  then if inp <= #"9"
                      then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                      else yyAction53(strm, yyNO_MATCH)
                else if inp <= #"Z"
                  then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                  else yyAction53(strm, yyNO_MATCH)
            else if inp = #"i"
              then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
            else if inp < #"i"
              then if inp = #"a"
                  then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                else if inp < #"a"
                  then yyAction53(strm, yyNO_MATCH)
                else if inp = #"h"
                  then yyQ53(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                  else yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
            else if inp = #"u"
              then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
            else if inp < #"u"
              then if inp = #"t"
                  then yyQ54(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                  else yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
              else yyAction53(strm, yyNO_MATCH)
      (* end case *))
fun yyQ60 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction18(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"A"
              then yyQ48(strm', yyMATCH(strm, yyAction18, yyNO_MATCH))
            else if inp < #"A"
              then if inp = #"("
                  then yyAction18(strm, yyNO_MATCH)
                else if inp < #"("
                  then if inp = #"'"
                      then yyQ48(strm', yyMATCH(strm, yyAction18, yyNO_MATCH))
                      else yyAction18(strm, yyNO_MATCH)
                else if inp = #"0"
                  then yyQ48(strm', yyMATCH(strm, yyAction18, yyNO_MATCH))
                else if inp < #"0"
                  then yyAction18(strm, yyNO_MATCH)
                else if inp <= #"9"
                  then yyQ48(strm', yyMATCH(strm, yyAction18, yyNO_MATCH))
                  else yyAction18(strm, yyNO_MATCH)
            else if inp = #"`"
              then yyAction18(strm, yyNO_MATCH)
            else if inp < #"`"
              then if inp = #"["
                  then yyAction18(strm, yyNO_MATCH)
                else if inp < #"["
                  then yyQ48(strm', yyMATCH(strm, yyAction18, yyNO_MATCH))
                else if inp = #"_"
                  then yyQ48(strm', yyMATCH(strm, yyAction18, yyNO_MATCH))
                  else yyAction18(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ48(strm', yyMATCH(strm, yyAction18, yyNO_MATCH))
              else yyAction18(strm, yyNO_MATCH)
      (* end case *))
fun yyQ59 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction53(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction53(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #"0"
                  then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                else if inp < #"0"
                  then if inp = #"'"
                      then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                      else yyAction53(strm, yyNO_MATCH)
                else if inp = #":"
                  then yyAction53(strm, yyNO_MATCH)
                else if inp < #":"
                  then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction53(strm, yyNO_MATCH)
                  else yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
            else if inp = #"a"
              then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
            else if inp < #"a"
              then if inp = #"_"
                  then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                  else yyAction53(strm, yyNO_MATCH)
            else if inp = #"e"
              then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
            else if inp < #"e"
              then if inp = #"d"
                  then yyQ60(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                  else yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
              else yyAction53(strm, yyNO_MATCH)
      (* end case *))
fun yyQ58 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction53(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction53(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #"0"
                  then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                else if inp < #"0"
                  then if inp = #"'"
                      then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                      else yyAction53(strm, yyNO_MATCH)
                else if inp = #":"
                  then yyAction53(strm, yyNO_MATCH)
                else if inp < #":"
                  then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction53(strm, yyNO_MATCH)
                  else yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
            else if inp = #"a"
              then yyQ59(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
            else if inp < #"a"
              then if inp = #"_"
                  then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                  else yyAction53(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
              else yyAction53(strm, yyNO_MATCH)
      (* end case *))
fun yyQ66 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction22(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"A"
              then yyQ48(strm', yyMATCH(strm, yyAction22, yyNO_MATCH))
            else if inp < #"A"
              then if inp = #"("
                  then yyAction22(strm, yyNO_MATCH)
                else if inp < #"("
                  then if inp = #"'"
                      then yyQ48(strm', yyMATCH(strm, yyAction22, yyNO_MATCH))
                      else yyAction22(strm, yyNO_MATCH)
                else if inp = #"0"
                  then yyQ48(strm', yyMATCH(strm, yyAction22, yyNO_MATCH))
                else if inp < #"0"
                  then yyAction22(strm, yyNO_MATCH)
                else if inp <= #"9"
                  then yyQ48(strm', yyMATCH(strm, yyAction22, yyNO_MATCH))
                  else yyAction22(strm, yyNO_MATCH)
            else if inp = #"`"
              then yyAction22(strm, yyNO_MATCH)
            else if inp < #"`"
              then if inp = #"["
                  then yyAction22(strm, yyNO_MATCH)
                else if inp < #"["
                  then yyQ48(strm', yyMATCH(strm, yyAction22, yyNO_MATCH))
                else if inp = #"_"
                  then yyQ48(strm', yyMATCH(strm, yyAction22, yyNO_MATCH))
                  else yyAction22(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ48(strm', yyMATCH(strm, yyAction22, yyNO_MATCH))
              else yyAction22(strm, yyNO_MATCH)
      (* end case *))
fun yyQ65 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction53(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction53(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #"0"
                  then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                else if inp < #"0"
                  then if inp = #"'"
                      then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                      else yyAction53(strm, yyNO_MATCH)
                else if inp = #":"
                  then yyAction53(strm, yyNO_MATCH)
                else if inp < #":"
                  then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction53(strm, yyNO_MATCH)
                  else yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
            else if inp = #"a"
              then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
            else if inp < #"a"
              then if inp = #"_"
                  then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                  else yyAction53(strm, yyNO_MATCH)
            else if inp = #"m"
              then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
            else if inp < #"m"
              then if inp = #"l"
                  then yyQ66(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                  else yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
              else yyAction53(strm, yyNO_MATCH)
      (* end case *))
fun yyQ64 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction53(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction53(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #"0"
                  then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                else if inp < #"0"
                  then if inp = #"'"
                      then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                      else yyAction53(strm, yyNO_MATCH)
                else if inp = #":"
                  then yyAction53(strm, yyNO_MATCH)
                else if inp < #":"
                  then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction53(strm, yyNO_MATCH)
                  else yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
            else if inp = #"a"
              then yyQ65(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
            else if inp < #"a"
              then if inp = #"_"
                  then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                  else yyAction53(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
              else yyAction53(strm, yyNO_MATCH)
      (* end case *))
fun yyQ63 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction53(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction53(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #"0"
                  then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                else if inp < #"0"
                  then if inp = #"'"
                      then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                      else yyAction53(strm, yyNO_MATCH)
                else if inp = #":"
                  then yyAction53(strm, yyNO_MATCH)
                else if inp < #":"
                  then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction53(strm, yyNO_MATCH)
                  else yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
            else if inp = #"a"
              then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
            else if inp < #"a"
              then if inp = #"_"
                  then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                  else yyAction53(strm, yyNO_MATCH)
            else if inp = #"o"
              then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
            else if inp < #"o"
              then if inp = #"n"
                  then yyQ64(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                  else yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
              else yyAction53(strm, yyNO_MATCH)
      (* end case *))
fun yyQ62 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction53(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction53(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #"0"
                  then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                else if inp < #"0"
                  then if inp = #"'"
                      then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                      else yyAction53(strm, yyNO_MATCH)
                else if inp = #":"
                  then yyAction53(strm, yyNO_MATCH)
                else if inp < #":"
                  then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction53(strm, yyNO_MATCH)
                  else yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
            else if inp = #"a"
              then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
            else if inp < #"a"
              then if inp = #"_"
                  then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                  else yyAction53(strm, yyNO_MATCH)
            else if inp = #"p"
              then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
            else if inp < #"p"
              then if inp = #"o"
                  then yyQ63(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                  else yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
              else yyAction53(strm, yyNO_MATCH)
      (* end case *))
fun yyQ61 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction11(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction11(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #"0"
                  then yyQ48(strm', yyMATCH(strm, yyAction11, yyNO_MATCH))
                else if inp < #"0"
                  then if inp = #"'"
                      then yyQ48(strm', yyMATCH(strm, yyAction11, yyNO_MATCH))
                      else yyAction11(strm, yyNO_MATCH)
                else if inp = #":"
                  then yyAction11(strm, yyNO_MATCH)
                else if inp < #":"
                  then yyQ48(strm', yyMATCH(strm, yyAction11, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction11(strm, yyNO_MATCH)
                  else yyQ48(strm', yyMATCH(strm, yyAction11, yyNO_MATCH))
            else if inp = #"a"
              then yyQ48(strm', yyMATCH(strm, yyAction11, yyNO_MATCH))
            else if inp < #"a"
              then if inp = #"_"
                  then yyQ48(strm', yyMATCH(strm, yyAction11, yyNO_MATCH))
                  else yyAction11(strm, yyNO_MATCH)
            else if inp = #"j"
              then yyQ48(strm', yyMATCH(strm, yyAction11, yyNO_MATCH))
            else if inp < #"j"
              then if inp = #"i"
                  then yyQ62(strm', yyMATCH(strm, yyAction11, yyNO_MATCH))
                  else yyQ48(strm', yyMATCH(strm, yyAction11, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ48(strm', yyMATCH(strm, yyAction11, yyNO_MATCH))
              else yyAction11(strm, yyNO_MATCH)
      (* end case *))
fun yyQ57 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction53(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction53(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #"0"
                  then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                else if inp < #"0"
                  then if inp = #"'"
                      then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                      else yyAction53(strm, yyNO_MATCH)
                else if inp = #":"
                  then yyAction53(strm, yyNO_MATCH)
                else if inp < #":"
                  then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction53(strm, yyNO_MATCH)
                  else yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
            else if inp = #"a"
              then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
            else if inp < #"a"
              then if inp = #"_"
                  then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                  else yyAction53(strm, yyNO_MATCH)
            else if inp = #"u"
              then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
            else if inp < #"u"
              then if inp = #"t"
                  then yyQ61(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                  else yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
              else yyAction53(strm, yyNO_MATCH)
      (* end case *))
fun yyQ35 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction53(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"_"
              then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
            else if inp < #"_"
              then if inp = #"0"
                  then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                else if inp < #"0"
                  then if inp = #"'"
                      then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                      else yyAction53(strm, yyNO_MATCH)
                else if inp = #"A"
                  then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                else if inp < #"A"
                  then if inp <= #"9"
                      then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                      else yyAction53(strm, yyNO_MATCH)
                else if inp <= #"Z"
                  then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                  else yyAction53(strm, yyNO_MATCH)
            else if inp = #"e"
              then yyQ58(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
            else if inp < #"e"
              then if inp = #"a"
                  then yyQ57(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                else if inp = #"`"
                  then yyAction53(strm, yyNO_MATCH)
                  else yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
              else yyAction53(strm, yyNO_MATCH)
      (* end case *))
fun yyQ75 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction17(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"A"
              then yyQ48(strm', yyMATCH(strm, yyAction17, yyNO_MATCH))
            else if inp < #"A"
              then if inp = #"("
                  then yyAction17(strm, yyNO_MATCH)
                else if inp < #"("
                  then if inp = #"'"
                      then yyQ48(strm', yyMATCH(strm, yyAction17, yyNO_MATCH))
                      else yyAction17(strm, yyNO_MATCH)
                else if inp = #"0"
                  then yyQ48(strm', yyMATCH(strm, yyAction17, yyNO_MATCH))
                else if inp < #"0"
                  then yyAction17(strm, yyNO_MATCH)
                else if inp <= #"9"
                  then yyQ48(strm', yyMATCH(strm, yyAction17, yyNO_MATCH))
                  else yyAction17(strm, yyNO_MATCH)
            else if inp = #"`"
              then yyAction17(strm, yyNO_MATCH)
            else if inp < #"`"
              then if inp = #"["
                  then yyAction17(strm, yyNO_MATCH)
                else if inp < #"["
                  then yyQ48(strm', yyMATCH(strm, yyAction17, yyNO_MATCH))
                else if inp = #"_"
                  then yyQ48(strm', yyMATCH(strm, yyAction17, yyNO_MATCH))
                  else yyAction17(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ48(strm', yyMATCH(strm, yyAction17, yyNO_MATCH))
              else yyAction17(strm, yyNO_MATCH)
      (* end case *))
fun yyQ74 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction53(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction53(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #"0"
                  then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                else if inp < #"0"
                  then if inp = #"'"
                      then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                      else yyAction53(strm, yyNO_MATCH)
                else if inp = #":"
                  then yyAction53(strm, yyNO_MATCH)
                else if inp < #":"
                  then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction53(strm, yyNO_MATCH)
                  else yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
            else if inp = #"a"
              then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
            else if inp < #"a"
              then if inp = #"_"
                  then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                  else yyAction53(strm, yyNO_MATCH)
            else if inp = #"f"
              then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
            else if inp < #"f"
              then if inp = #"e"
                  then yyQ75(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                  else yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
              else yyAction53(strm, yyNO_MATCH)
      (* end case *))
fun yyQ73 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction53(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction53(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #"0"
                  then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                else if inp < #"0"
                  then if inp = #"'"
                      then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                      else yyAction53(strm, yyNO_MATCH)
                else if inp = #":"
                  then yyAction53(strm, yyNO_MATCH)
                else if inp < #":"
                  then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction53(strm, yyNO_MATCH)
                  else yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
            else if inp = #"a"
              then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
            else if inp < #"a"
              then if inp = #"_"
                  then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                  else yyAction53(strm, yyNO_MATCH)
            else if inp = #"s"
              then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
            else if inp < #"s"
              then if inp = #"r"
                  then yyQ74(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                  else yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
              else yyAction53(strm, yyNO_MATCH)
      (* end case *))
fun yyQ72 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction53(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction53(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #"0"
                  then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                else if inp < #"0"
                  then if inp = #"'"
                      then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                      else yyAction53(strm, yyNO_MATCH)
                else if inp = #":"
                  then yyAction53(strm, yyNO_MATCH)
                else if inp < #":"
                  then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction53(strm, yyNO_MATCH)
                  else yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
            else if inp = #"a"
              then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
            else if inp < #"a"
              then if inp = #"_"
                  then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                  else yyAction53(strm, yyNO_MATCH)
            else if inp = #"v"
              then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
            else if inp < #"v"
              then if inp = #"u"
                  then yyQ73(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                  else yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
              else yyAction53(strm, yyNO_MATCH)
      (* end case *))
fun yyQ71 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction53(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction53(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #"0"
                  then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                else if inp < #"0"
                  then if inp = #"'"
                      then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                      else yyAction53(strm, yyNO_MATCH)
                else if inp = #":"
                  then yyAction53(strm, yyNO_MATCH)
                else if inp < #":"
                  then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction53(strm, yyNO_MATCH)
                  else yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
            else if inp = #"a"
              then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
            else if inp < #"a"
              then if inp = #"_"
                  then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                  else yyAction53(strm, yyNO_MATCH)
            else if inp = #"e"
              then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
            else if inp < #"e"
              then if inp = #"d"
                  then yyQ72(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                  else yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
              else yyAction53(strm, yyNO_MATCH)
      (* end case *))
fun yyQ70 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction53(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction53(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #"0"
                  then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                else if inp < #"0"
                  then if inp = #"'"
                      then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                      else yyAction53(strm, yyNO_MATCH)
                else if inp = #":"
                  then yyAction53(strm, yyNO_MATCH)
                else if inp < #":"
                  then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction53(strm, yyNO_MATCH)
                  else yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
            else if inp = #"a"
              then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
            else if inp < #"a"
              then if inp = #"_"
                  then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                  else yyAction53(strm, yyNO_MATCH)
            else if inp = #"f"
              then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
            else if inp < #"f"
              then if inp = #"e"
                  then yyQ71(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                  else yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
              else yyAction53(strm, yyNO_MATCH)
      (* end case *))
fun yyQ69 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction53(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction53(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #"0"
                  then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                else if inp < #"0"
                  then if inp = #"'"
                      then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                      else yyAction53(strm, yyNO_MATCH)
                else if inp = #":"
                  then yyAction53(strm, yyNO_MATCH)
                else if inp < #":"
                  then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction53(strm, yyNO_MATCH)
                  else yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
            else if inp = #"a"
              then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
            else if inp < #"a"
              then if inp = #"_"
                  then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                  else yyAction53(strm, yyNO_MATCH)
            else if inp = #"d"
              then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
            else if inp < #"d"
              then if inp = #"c"
                  then yyQ70(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                  else yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
              else yyAction53(strm, yyNO_MATCH)
      (* end case *))
fun yyQ77 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction25(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"A"
              then yyQ48(strm', yyMATCH(strm, yyAction25, yyNO_MATCH))
            else if inp < #"A"
              then if inp = #"("
                  then yyAction25(strm, yyNO_MATCH)
                else if inp < #"("
                  then if inp = #"'"
                      then yyQ48(strm', yyMATCH(strm, yyAction25, yyNO_MATCH))
                      else yyAction25(strm, yyNO_MATCH)
                else if inp = #"0"
                  then yyQ48(strm', yyMATCH(strm, yyAction25, yyNO_MATCH))
                else if inp < #"0"
                  then yyAction25(strm, yyNO_MATCH)
                else if inp <= #"9"
                  then yyQ48(strm', yyMATCH(strm, yyAction25, yyNO_MATCH))
                  else yyAction25(strm, yyNO_MATCH)
            else if inp = #"`"
              then yyAction25(strm, yyNO_MATCH)
            else if inp < #"`"
              then if inp = #"["
                  then yyAction25(strm, yyNO_MATCH)
                else if inp < #"["
                  then yyQ48(strm', yyMATCH(strm, yyAction25, yyNO_MATCH))
                else if inp = #"_"
                  then yyQ48(strm', yyMATCH(strm, yyAction25, yyNO_MATCH))
                  else yyAction25(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ48(strm', yyMATCH(strm, yyAction25, yyNO_MATCH))
              else yyAction25(strm, yyNO_MATCH)
      (* end case *))
fun yyQ76 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction53(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction53(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #"0"
                  then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                else if inp < #"0"
                  then if inp = #"'"
                      then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                      else yyAction53(strm, yyNO_MATCH)
                else if inp = #":"
                  then yyAction53(strm, yyNO_MATCH)
                else if inp < #":"
                  then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction53(strm, yyNO_MATCH)
                  else yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
            else if inp = #"a"
              then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
            else if inp < #"a"
              then if inp = #"_"
                  then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                  else yyAction53(strm, yyNO_MATCH)
            else if inp = #"u"
              then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
            else if inp < #"u"
              then if inp = #"t"
                  then yyQ77(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                  else yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
              else yyAction53(strm, yyNO_MATCH)
      (* end case *))
fun yyQ68 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction53(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction53(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #"0"
                  then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                else if inp < #"0"
                  then if inp = #"'"
                      then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                      else yyAction53(strm, yyNO_MATCH)
                else if inp = #":"
                  then yyAction53(strm, yyNO_MATCH)
                else if inp < #":"
                  then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction53(strm, yyNO_MATCH)
                  else yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
            else if inp = #"a"
              then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
            else if inp < #"a"
              then if inp = #"_"
                  then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                  else yyAction53(strm, yyNO_MATCH)
            else if inp = #"o"
              then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
            else if inp < #"o"
              then if inp = #"n"
                  then yyQ76(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                  else yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
              else yyAction53(strm, yyNO_MATCH)
      (* end case *))
fun yyQ67 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction53(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"_"
              then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
            else if inp < #"_"
              then if inp = #"0"
                  then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                else if inp < #"0"
                  then if inp = #"'"
                      then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                      else yyAction53(strm, yyNO_MATCH)
                else if inp = #"A"
                  then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                else if inp < #"A"
                  then if inp <= #"9"
                      then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                      else yyAction53(strm, yyNO_MATCH)
                else if inp <= #"Z"
                  then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                  else yyAction53(strm, yyNO_MATCH)
            else if inp = #"j"
              then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
            else if inp < #"j"
              then if inp = #"a"
                  then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                else if inp < #"a"
                  then yyAction53(strm, yyNO_MATCH)
                else if inp = #"i"
                  then yyQ68(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                  else yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
            else if inp = #"p"
              then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
            else if inp < #"p"
              then if inp = #"o"
                  then yyQ69(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                  else yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
              else yyAction53(strm, yyNO_MATCH)
      (* end case *))
fun yyQ34 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction53(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction53(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #"0"
                  then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                else if inp < #"0"
                  then if inp = #"'"
                      then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                      else yyAction53(strm, yyNO_MATCH)
                else if inp = #":"
                  then yyAction53(strm, yyNO_MATCH)
                else if inp < #":"
                  then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction53(strm, yyNO_MATCH)
                  else yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
            else if inp = #"a"
              then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
            else if inp < #"a"
              then if inp = #"_"
                  then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                  else yyAction53(strm, yyNO_MATCH)
            else if inp = #"s"
              then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
            else if inp < #"s"
              then if inp = #"r"
                  then yyQ67(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                  else yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
              else yyAction53(strm, yyNO_MATCH)
      (* end case *))
fun yyQ79 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction16(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"A"
              then yyQ48(strm', yyMATCH(strm, yyAction16, yyNO_MATCH))
            else if inp < #"A"
              then if inp = #"("
                  then yyAction16(strm, yyNO_MATCH)
                else if inp < #"("
                  then if inp = #"'"
                      then yyQ48(strm', yyMATCH(strm, yyAction16, yyNO_MATCH))
                      else yyAction16(strm, yyNO_MATCH)
                else if inp = #"0"
                  then yyQ48(strm', yyMATCH(strm, yyAction16, yyNO_MATCH))
                else if inp < #"0"
                  then yyAction16(strm, yyNO_MATCH)
                else if inp <= #"9"
                  then yyQ48(strm', yyMATCH(strm, yyAction16, yyNO_MATCH))
                  else yyAction16(strm, yyNO_MATCH)
            else if inp = #"`"
              then yyAction16(strm, yyNO_MATCH)
            else if inp < #"`"
              then if inp = #"["
                  then yyAction16(strm, yyNO_MATCH)
                else if inp < #"["
                  then yyQ48(strm', yyMATCH(strm, yyAction16, yyNO_MATCH))
                else if inp = #"_"
                  then yyQ48(strm', yyMATCH(strm, yyAction16, yyNO_MATCH))
                  else yyAction16(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ48(strm', yyMATCH(strm, yyAction16, yyNO_MATCH))
              else yyAction16(strm, yyNO_MATCH)
      (* end case *))
fun yyQ78 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction14(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"A"
              then yyQ48(strm', yyMATCH(strm, yyAction14, yyNO_MATCH))
            else if inp < #"A"
              then if inp = #"("
                  then yyAction14(strm, yyNO_MATCH)
                else if inp < #"("
                  then if inp = #"'"
                      then yyQ48(strm', yyMATCH(strm, yyAction14, yyNO_MATCH))
                      else yyAction14(strm, yyNO_MATCH)
                else if inp = #"0"
                  then yyQ48(strm', yyMATCH(strm, yyAction14, yyNO_MATCH))
                else if inp < #"0"
                  then yyAction14(strm, yyNO_MATCH)
                else if inp <= #"9"
                  then yyQ48(strm', yyMATCH(strm, yyAction14, yyNO_MATCH))
                  else yyAction14(strm, yyNO_MATCH)
            else if inp = #"`"
              then yyAction14(strm, yyNO_MATCH)
            else if inp < #"`"
              then if inp = #"["
                  then yyAction14(strm, yyNO_MATCH)
                else if inp < #"["
                  then yyQ48(strm', yyMATCH(strm, yyAction14, yyNO_MATCH))
                else if inp = #"_"
                  then yyQ48(strm', yyMATCH(strm, yyAction14, yyNO_MATCH))
                  else yyAction14(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ48(strm', yyMATCH(strm, yyAction14, yyNO_MATCH))
              else yyAction14(strm, yyNO_MATCH)
      (* end case *))
fun yyQ33 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction53(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"_"
              then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
            else if inp < #"_"
              then if inp = #"0"
                  then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                else if inp < #"0"
                  then if inp = #"'"
                      then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                      else yyAction53(strm, yyNO_MATCH)
                else if inp = #"A"
                  then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                else if inp < #"A"
                  then if inp <= #"9"
                      then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                      else yyAction53(strm, yyNO_MATCH)
                else if inp <= #"Z"
                  then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                  else yyAction53(strm, yyNO_MATCH)
            else if inp = #"e"
              then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
            else if inp < #"e"
              then if inp = #"a"
                  then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                else if inp < #"a"
                  then yyAction53(strm, yyNO_MATCH)
                else if inp = #"d"
                  then yyQ78(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                  else yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
            else if inp = #"s"
              then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
            else if inp < #"s"
              then if inp = #"r"
                  then yyQ79(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                  else yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
              else yyAction53(strm, yyNO_MATCH)
      (* end case *))
fun yyQ86 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction20(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"A"
              then yyQ48(strm', yyMATCH(strm, yyAction20, yyNO_MATCH))
            else if inp < #"A"
              then if inp = #"("
                  then yyAction20(strm, yyNO_MATCH)
                else if inp < #"("
                  then if inp = #"'"
                      then yyQ48(strm', yyMATCH(strm, yyAction20, yyNO_MATCH))
                      else yyAction20(strm, yyNO_MATCH)
                else if inp = #"0"
                  then yyQ48(strm', yyMATCH(strm, yyAction20, yyNO_MATCH))
                else if inp < #"0"
                  then yyAction20(strm, yyNO_MATCH)
                else if inp <= #"9"
                  then yyQ48(strm', yyMATCH(strm, yyAction20, yyNO_MATCH))
                  else yyAction20(strm, yyNO_MATCH)
            else if inp = #"`"
              then yyAction20(strm, yyNO_MATCH)
            else if inp < #"`"
              then if inp = #"["
                  then yyAction20(strm, yyNO_MATCH)
                else if inp < #"["
                  then yyQ48(strm', yyMATCH(strm, yyAction20, yyNO_MATCH))
                else if inp = #"_"
                  then yyQ48(strm', yyMATCH(strm, yyAction20, yyNO_MATCH))
                  else yyAction20(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ48(strm', yyMATCH(strm, yyAction20, yyNO_MATCH))
              else yyAction20(strm, yyNO_MATCH)
      (* end case *))
fun yyQ85 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction53(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction53(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #"0"
                  then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                else if inp < #"0"
                  then if inp = #"'"
                      then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                      else yyAction53(strm, yyNO_MATCH)
                else if inp = #":"
                  then yyAction53(strm, yyNO_MATCH)
                else if inp < #":"
                  then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction53(strm, yyNO_MATCH)
                  else yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
            else if inp = #"a"
              then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
            else if inp < #"a"
              then if inp = #"_"
                  then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                  else yyAction53(strm, yyNO_MATCH)
            else if inp = #"u"
              then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
            else if inp < #"u"
              then if inp = #"t"
                  then yyQ86(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                  else yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
              else yyAction53(strm, yyNO_MATCH)
      (* end case *))
fun yyQ84 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction53(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction53(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #"0"
                  then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                else if inp < #"0"
                  then if inp = #"'"
                      then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                      else yyAction53(strm, yyNO_MATCH)
                else if inp = #":"
                  then yyAction53(strm, yyNO_MATCH)
                else if inp < #":"
                  then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction53(strm, yyNO_MATCH)
                  else yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
            else if inp = #"a"
              then yyQ85(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
            else if inp < #"a"
              then if inp = #"_"
                  then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                  else yyAction53(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
              else yyAction53(strm, yyNO_MATCH)
      (* end case *))
fun yyQ83 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction53(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction53(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #"0"
                  then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                else if inp < #"0"
                  then if inp = #"'"
                      then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                      else yyAction53(strm, yyNO_MATCH)
                else if inp = #":"
                  then yyAction53(strm, yyNO_MATCH)
                else if inp < #":"
                  then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction53(strm, yyNO_MATCH)
                  else yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
            else if inp = #"a"
              then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
            else if inp < #"a"
              then if inp = #"_"
                  then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                  else yyAction53(strm, yyNO_MATCH)
            else if inp = #"s"
              then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
            else if inp < #"s"
              then if inp = #"r"
                  then yyQ84(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                  else yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
              else yyAction53(strm, yyNO_MATCH)
      (* end case *))
fun yyQ82 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction53(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"A"
              then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
            else if inp < #"A"
              then if inp = #"("
                  then yyAction53(strm, yyNO_MATCH)
                else if inp < #"("
                  then if inp = #"'"
                      then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                      else yyAction53(strm, yyNO_MATCH)
                else if inp = #"0"
                  then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                else if inp < #"0"
                  then yyAction53(strm, yyNO_MATCH)
                else if inp <= #"9"
                  then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                  else yyAction53(strm, yyNO_MATCH)
            else if inp = #"`"
              then yyAction53(strm, yyNO_MATCH)
            else if inp < #"`"
              then if inp = #"["
                  then yyAction53(strm, yyNO_MATCH)
                else if inp < #"["
                  then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                else if inp = #"_"
                  then yyQ83(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                  else yyAction53(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
              else yyAction53(strm, yyNO_MATCH)
      (* end case *))
fun yyQ81 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction53(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction53(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #"0"
                  then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                else if inp < #"0"
                  then if inp = #"'"
                      then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                      else yyAction53(strm, yyNO_MATCH)
                else if inp = #":"
                  then yyAction53(strm, yyNO_MATCH)
                else if inp < #":"
                  then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction53(strm, yyNO_MATCH)
                  else yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
            else if inp = #"a"
              then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
            else if inp < #"a"
              then if inp = #"_"
                  then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                  else yyAction53(strm, yyNO_MATCH)
            else if inp = #"f"
              then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
            else if inp < #"f"
              then if inp = #"e"
                  then yyQ82(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                  else yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
              else yyAction53(strm, yyNO_MATCH)
      (* end case *))
fun yyQ80 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction53(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction53(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #"0"
                  then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                else if inp < #"0"
                  then if inp = #"'"
                      then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                      else yyAction53(strm, yyNO_MATCH)
                else if inp = #":"
                  then yyAction53(strm, yyNO_MATCH)
                else if inp < #":"
                  then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction53(strm, yyNO_MATCH)
                  else yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
            else if inp = #"a"
              then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
            else if inp < #"a"
              then if inp = #"_"
                  then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                  else yyAction53(strm, yyNO_MATCH)
            else if inp = #"l"
              then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
            else if inp < #"l"
              then if inp = #"k"
                  then yyQ81(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                  else yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
              else yyAction53(strm, yyNO_MATCH)
      (* end case *))
fun yyQ32 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction53(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction53(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #"0"
                  then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                else if inp < #"0"
                  then if inp = #"'"
                      then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                      else yyAction53(strm, yyNO_MATCH)
                else if inp = #":"
                  then yyAction53(strm, yyNO_MATCH)
                else if inp < #":"
                  then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction53(strm, yyNO_MATCH)
                  else yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
            else if inp = #"a"
              then yyQ80(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
            else if inp < #"a"
              then if inp = #"_"
                  then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                  else yyAction53(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
              else yyAction53(strm, yyNO_MATCH)
      (* end case *))
fun yyQ94 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction34(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"A"
              then yyQ48(strm', yyMATCH(strm, yyAction34, yyNO_MATCH))
            else if inp < #"A"
              then if inp = #"("
                  then yyAction34(strm, yyNO_MATCH)
                else if inp < #"("
                  then if inp = #"'"
                      then yyQ48(strm', yyMATCH(strm, yyAction34, yyNO_MATCH))
                      else yyAction34(strm, yyNO_MATCH)
                else if inp = #"0"
                  then yyQ48(strm', yyMATCH(strm, yyAction34, yyNO_MATCH))
                else if inp < #"0"
                  then yyAction34(strm, yyNO_MATCH)
                else if inp <= #"9"
                  then yyQ48(strm', yyMATCH(strm, yyAction34, yyNO_MATCH))
                  else yyAction34(strm, yyNO_MATCH)
            else if inp = #"`"
              then yyAction34(strm, yyNO_MATCH)
            else if inp < #"`"
              then if inp = #"["
                  then yyAction34(strm, yyNO_MATCH)
                else if inp < #"["
                  then yyQ48(strm', yyMATCH(strm, yyAction34, yyNO_MATCH))
                else if inp = #"_"
                  then yyQ48(strm', yyMATCH(strm, yyAction34, yyNO_MATCH))
                  else yyAction34(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ48(strm', yyMATCH(strm, yyAction34, yyNO_MATCH))
              else yyAction34(strm, yyNO_MATCH)
      (* end case *))
fun yyQ93 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction53(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction53(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #"0"
                  then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                else if inp < #"0"
                  then if inp = #"'"
                      then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                      else yyAction53(strm, yyNO_MATCH)
                else if inp = #":"
                  then yyAction53(strm, yyNO_MATCH)
                else if inp < #":"
                  then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction53(strm, yyNO_MATCH)
                  else yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
            else if inp = #"a"
              then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
            else if inp < #"a"
              then if inp = #"_"
                  then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                  else yyAction53(strm, yyNO_MATCH)
            else if inp = #"f"
              then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
            else if inp < #"f"
              then if inp = #"e"
                  then yyQ94(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                  else yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
              else yyAction53(strm, yyNO_MATCH)
      (* end case *))
fun yyQ92 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction53(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction53(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #"0"
                  then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                else if inp < #"0"
                  then if inp = #"'"
                      then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                      else yyAction53(strm, yyNO_MATCH)
                else if inp = #":"
                  then yyAction53(strm, yyNO_MATCH)
                else if inp < #":"
                  then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction53(strm, yyNO_MATCH)
                  else yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
            else if inp = #"a"
              then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
            else if inp < #"a"
              then if inp = #"_"
                  then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                  else yyAction53(strm, yyNO_MATCH)
            else if inp = #"t"
              then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
            else if inp < #"t"
              then if inp = #"s"
                  then yyQ93(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                  else yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
              else yyAction53(strm, yyNO_MATCH)
      (* end case *))
fun yyQ91 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction53(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction53(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #"0"
                  then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                else if inp < #"0"
                  then if inp = #"'"
                      then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                      else yyAction53(strm, yyNO_MATCH)
                else if inp = #":"
                  then yyAction53(strm, yyNO_MATCH)
                else if inp < #":"
                  then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction53(strm, yyNO_MATCH)
                  else yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
            else if inp = #"a"
              then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
            else if inp < #"a"
              then if inp = #"_"
                  then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                  else yyAction53(strm, yyNO_MATCH)
            else if inp = #"s"
              then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
            else if inp < #"s"
              then if inp = #"r"
                  then yyQ92(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                  else yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
              else yyAction53(strm, yyNO_MATCH)
      (* end case *))
fun yyQ90 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction53(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction53(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #"0"
                  then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                else if inp < #"0"
                  then if inp = #"'"
                      then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                      else yyAction53(strm, yyNO_MATCH)
                else if inp = #":"
                  then yyAction53(strm, yyNO_MATCH)
                else if inp < #":"
                  then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction53(strm, yyNO_MATCH)
                  else yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
            else if inp = #"a"
              then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
            else if inp < #"a"
              then if inp = #"_"
                  then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                  else yyAction53(strm, yyNO_MATCH)
            else if inp = #"f"
              then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
            else if inp < #"f"
              then if inp = #"e"
                  then yyQ91(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                  else yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
              else yyAction53(strm, yyNO_MATCH)
      (* end case *))
fun yyQ98 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction23(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"A"
              then yyQ48(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
            else if inp < #"A"
              then if inp = #"("
                  then yyAction23(strm, yyNO_MATCH)
                else if inp < #"("
                  then if inp = #"'"
                      then yyQ48(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
                      else yyAction23(strm, yyNO_MATCH)
                else if inp = #"0"
                  then yyQ48(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
                else if inp < #"0"
                  then yyAction23(strm, yyNO_MATCH)
                else if inp <= #"9"
                  then yyQ48(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
                  else yyAction23(strm, yyNO_MATCH)
            else if inp = #"`"
              then yyAction23(strm, yyNO_MATCH)
            else if inp < #"`"
              then if inp = #"["
                  then yyAction23(strm, yyNO_MATCH)
                else if inp < #"["
                  then yyQ48(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
                else if inp = #"_"
                  then yyQ48(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
                  else yyAction23(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ48(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
              else yyAction23(strm, yyNO_MATCH)
      (* end case *))
fun yyQ97 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction53(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction53(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #"0"
                  then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                else if inp < #"0"
                  then if inp = #"'"
                      then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                      else yyAction53(strm, yyNO_MATCH)
                else if inp = #":"
                  then yyAction53(strm, yyNO_MATCH)
                else if inp < #":"
                  then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction53(strm, yyNO_MATCH)
                  else yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
            else if inp = #"a"
              then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
            else if inp < #"a"
              then if inp = #"_"
                  then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                  else yyAction53(strm, yyNO_MATCH)
            else if inp = #"s"
              then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
            else if inp < #"s"
              then if inp = #"r"
                  then yyQ98(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                  else yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
              else yyAction53(strm, yyNO_MATCH)
      (* end case *))
fun yyQ96 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction53(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction53(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #"0"
                  then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                else if inp < #"0"
                  then if inp = #"'"
                      then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                      else yyAction53(strm, yyNO_MATCH)
                else if inp = #":"
                  then yyAction53(strm, yyNO_MATCH)
                else if inp < #":"
                  then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction53(strm, yyNO_MATCH)
                  else yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
            else if inp = #"a"
              then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
            else if inp < #"a"
              then if inp = #"_"
                  then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                  else yyAction53(strm, yyNO_MATCH)
            else if inp = #"f"
              then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
            else if inp < #"f"
              then if inp = #"e"
                  then yyQ97(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                  else yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
              else yyAction53(strm, yyNO_MATCH)
      (* end case *))
fun yyQ95 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction53(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction53(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #"0"
                  then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                else if inp < #"0"
                  then if inp = #"'"
                      then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                      else yyAction53(strm, yyNO_MATCH)
                else if inp = #":"
                  then yyAction53(strm, yyNO_MATCH)
                else if inp < #":"
                  then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction53(strm, yyNO_MATCH)
                  else yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
            else if inp = #"a"
              then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
            else if inp < #"a"
              then if inp = #"_"
                  then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                  else yyAction53(strm, yyNO_MATCH)
            else if inp = #"h"
              then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
            else if inp < #"h"
              then if inp = #"g"
                  then yyQ96(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                  else yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
              else yyAction53(strm, yyNO_MATCH)
      (* end case *))
fun yyQ89 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction53(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction53(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #"0"
                  then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                else if inp < #"0"
                  then if inp = #"'"
                      then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                      else yyAction53(strm, yyNO_MATCH)
                else if inp = #":"
                  then yyAction53(strm, yyNO_MATCH)
                else if inp < #":"
                  then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction53(strm, yyNO_MATCH)
                  else yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
            else if inp = #"a"
              then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
            else if inp < #"a"
              then if inp = #"_"
                  then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                  else yyAction53(strm, yyNO_MATCH)
            else if inp = #"f"
              then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
            else if inp < #"f"
              then if inp = #"e"
                  then yyQ95(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                  else yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
              else yyAction53(strm, yyNO_MATCH)
      (* end case *))
fun yyQ88 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction53(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"_"
              then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
            else if inp < #"_"
              then if inp = #"0"
                  then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                else if inp < #"0"
                  then if inp = #"'"
                      then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                      else yyAction53(strm, yyNO_MATCH)
                else if inp = #"A"
                  then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                else if inp < #"A"
                  then if inp <= #"9"
                      then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                      else yyAction53(strm, yyNO_MATCH)
                else if inp <= #"Z"
                  then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                  else yyAction53(strm, yyNO_MATCH)
            else if inp = #"u"
              then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
            else if inp < #"u"
              then if inp = #"a"
                  then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                else if inp < #"a"
                  then yyAction53(strm, yyNO_MATCH)
                else if inp = #"t"
                  then yyQ89(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                  else yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
            else if inp = #"w"
              then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
            else if inp < #"w"
              then yyQ90(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
              else yyAction53(strm, yyNO_MATCH)
      (* end case *))
fun yyQ87 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction7(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"A"
              then yyQ48(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
            else if inp < #"A"
              then if inp = #"("
                  then yyAction7(strm, yyNO_MATCH)
                else if inp < #"("
                  then if inp = #"'"
                      then yyQ48(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
                      else yyAction7(strm, yyNO_MATCH)
                else if inp = #"0"
                  then yyQ48(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
                else if inp < #"0"
                  then yyAction7(strm, yyNO_MATCH)
                else if inp <= #"9"
                  then yyQ48(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
                  else yyAction7(strm, yyNO_MATCH)
            else if inp = #"`"
              then yyAction7(strm, yyNO_MATCH)
            else if inp < #"`"
              then if inp = #"["
                  then yyAction7(strm, yyNO_MATCH)
                else if inp < #"["
                  then yyQ48(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
                else if inp = #"_"
                  then yyQ48(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
                  else yyAction7(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ48(strm', yyMATCH(strm, yyAction7, yyNO_MATCH))
              else yyAction7(strm, yyNO_MATCH)
      (* end case *))
fun yyQ31 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction53(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"_"
              then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
            else if inp < #"_"
              then if inp = #"0"
                  then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                else if inp < #"0"
                  then if inp = #"'"
                      then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                      else yyAction53(strm, yyNO_MATCH)
                else if inp = #"A"
                  then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                else if inp < #"A"
                  then if inp <= #"9"
                      then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                      else yyAction53(strm, yyNO_MATCH)
                else if inp <= #"Z"
                  then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                  else yyAction53(strm, yyNO_MATCH)
            else if inp = #"g"
              then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
            else if inp < #"g"
              then if inp = #"a"
                  then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                else if inp < #"a"
                  then yyAction53(strm, yyNO_MATCH)
                else if inp = #"f"
                  then yyQ87(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                  else yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
            else if inp = #"o"
              then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
            else if inp < #"o"
              then if inp = #"n"
                  then yyQ88(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                  else yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
              else yyAction53(strm, yyNO_MATCH)
      (* end case *))
fun yyQ110 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction21(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"A"
              then yyQ48(strm', yyMATCH(strm, yyAction21, yyNO_MATCH))
            else if inp < #"A"
              then if inp = #"("
                  then yyAction21(strm, yyNO_MATCH)
                else if inp < #"("
                  then if inp = #"'"
                      then yyQ48(strm', yyMATCH(strm, yyAction21, yyNO_MATCH))
                      else yyAction21(strm, yyNO_MATCH)
                else if inp = #"0"
                  then yyQ48(strm', yyMATCH(strm, yyAction21, yyNO_MATCH))
                else if inp < #"0"
                  then yyAction21(strm, yyNO_MATCH)
                else if inp <= #"9"
                  then yyQ48(strm', yyMATCH(strm, yyAction21, yyNO_MATCH))
                  else yyAction21(strm, yyNO_MATCH)
            else if inp = #"`"
              then yyAction21(strm, yyNO_MATCH)
            else if inp < #"`"
              then if inp = #"["
                  then yyAction21(strm, yyNO_MATCH)
                else if inp < #"["
                  then yyQ48(strm', yyMATCH(strm, yyAction21, yyNO_MATCH))
                else if inp = #"_"
                  then yyQ48(strm', yyMATCH(strm, yyAction21, yyNO_MATCH))
                  else yyAction21(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ48(strm', yyMATCH(strm, yyAction21, yyNO_MATCH))
              else yyAction21(strm, yyNO_MATCH)
      (* end case *))
fun yyQ109 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction53(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction53(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #"0"
                  then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                else if inp < #"0"
                  then if inp = #"'"
                      then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                      else yyAction53(strm, yyNO_MATCH)
                else if inp = #":"
                  then yyAction53(strm, yyNO_MATCH)
                else if inp < #":"
                  then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction53(strm, yyNO_MATCH)
                  else yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
            else if inp = #"a"
              then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
            else if inp < #"a"
              then if inp = #"_"
                  then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                  else yyAction53(strm, yyNO_MATCH)
            else if inp = #"m"
              then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
            else if inp < #"m"
              then if inp = #"l"
                  then yyQ110(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                  else yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
              else yyAction53(strm, yyNO_MATCH)
      (* end case *))
fun yyQ108 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction53(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction53(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #"0"
                  then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                else if inp < #"0"
                  then if inp = #"'"
                      then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                      else yyAction53(strm, yyNO_MATCH)
                else if inp = #":"
                  then yyAction53(strm, yyNO_MATCH)
                else if inp < #":"
                  then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction53(strm, yyNO_MATCH)
                  else yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
            else if inp = #"a"
              then yyQ109(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
            else if inp < #"a"
              then if inp = #"_"
                  then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                  else yyAction53(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
              else yyAction53(strm, yyNO_MATCH)
      (* end case *))
fun yyQ107 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction53(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction53(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #"0"
                  then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                else if inp < #"0"
                  then if inp = #"'"
                      then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                      else yyAction53(strm, yyNO_MATCH)
                else if inp = #":"
                  then yyAction53(strm, yyNO_MATCH)
                else if inp < #":"
                  then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction53(strm, yyNO_MATCH)
                  else yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
            else if inp = #"a"
              then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
            else if inp < #"a"
              then if inp = #"_"
                  then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                  else yyAction53(strm, yyNO_MATCH)
            else if inp = #"n"
              then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
            else if inp < #"n"
              then if inp = #"m"
                  then yyQ108(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                  else yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
              else yyAction53(strm, yyNO_MATCH)
      (* end case *))
fun yyQ106 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction53(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction53(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #"0"
                  then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                else if inp < #"0"
                  then if inp = #"'"
                      then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                      else yyAction53(strm, yyNO_MATCH)
                else if inp = #":"
                  then yyAction53(strm, yyNO_MATCH)
                else if inp < #":"
                  then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction53(strm, yyNO_MATCH)
                  else yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
            else if inp = #"a"
              then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
            else if inp < #"a"
              then if inp = #"_"
                  then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                  else yyAction53(strm, yyNO_MATCH)
            else if inp = #"j"
              then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
            else if inp < #"j"
              then if inp = #"i"
                  then yyQ107(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                  else yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
              else yyAction53(strm, yyNO_MATCH)
      (* end case *))
fun yyQ105 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction53(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction53(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #"0"
                  then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                else if inp < #"0"
                  then if inp = #"'"
                      then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                      else yyAction53(strm, yyNO_MATCH)
                else if inp = #":"
                  then yyAction53(strm, yyNO_MATCH)
                else if inp < #":"
                  then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction53(strm, yyNO_MATCH)
                  else yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
            else if inp = #"a"
              then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
            else if inp < #"a"
              then if inp = #"_"
                  then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                  else yyAction53(strm, yyNO_MATCH)
            else if inp = #"d"
              then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
            else if inp < #"d"
              then if inp = #"c"
                  then yyQ106(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                  else yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
              else yyAction53(strm, yyNO_MATCH)
      (* end case *))
fun yyQ104 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction53(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction53(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #"0"
                  then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                else if inp < #"0"
                  then if inp = #"'"
                      then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                      else yyAction53(strm, yyNO_MATCH)
                else if inp = #":"
                  then yyAction53(strm, yyNO_MATCH)
                else if inp < #":"
                  then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction53(strm, yyNO_MATCH)
                  else yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
            else if inp = #"a"
              then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
            else if inp < #"a"
              then if inp = #"_"
                  then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                  else yyAction53(strm, yyNO_MATCH)
            else if inp = #"f"
              then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
            else if inp < #"f"
              then if inp = #"e"
                  then yyQ105(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                  else yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
              else yyAction53(strm, yyNO_MATCH)
      (* end case *))
fun yyQ103 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction53(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"D"
              then yyQ104(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
            else if inp < #"D"
              then if inp = #"0"
                  then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                else if inp < #"0"
                  then if inp = #"'"
                      then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                      else yyAction53(strm, yyNO_MATCH)
                else if inp = #":"
                  then yyAction53(strm, yyNO_MATCH)
                else if inp < #":"
                  then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction53(strm, yyNO_MATCH)
                  else yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
            else if inp = #"`"
              then yyAction53(strm, yyNO_MATCH)
            else if inp < #"`"
              then if inp = #"["
                  then yyAction53(strm, yyNO_MATCH)
                else if inp < #"["
                  then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                else if inp = #"_"
                  then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                  else yyAction53(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
              else yyAction53(strm, yyNO_MATCH)
      (* end case *))
fun yyQ102 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction53(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction53(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #"0"
                  then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                else if inp < #"0"
                  then if inp = #"'"
                      then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                      else yyAction53(strm, yyNO_MATCH)
                else if inp = #":"
                  then yyAction53(strm, yyNO_MATCH)
                else if inp < #":"
                  then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction53(strm, yyNO_MATCH)
                  else yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
            else if inp = #"a"
              then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
            else if inp < #"a"
              then if inp = #"_"
                  then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                  else yyAction53(strm, yyNO_MATCH)
            else if inp = #"n"
              then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
            else if inp < #"n"
              then if inp = #"m"
                  then yyQ103(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                  else yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
              else yyAction53(strm, yyNO_MATCH)
      (* end case *))
fun yyQ101 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction53(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction53(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #"0"
                  then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                else if inp < #"0"
                  then if inp = #"'"
                      then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                      else yyAction53(strm, yyNO_MATCH)
                else if inp = #":"
                  then yyAction53(strm, yyNO_MATCH)
                else if inp < #":"
                  then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction53(strm, yyNO_MATCH)
                  else yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
            else if inp = #"a"
              then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
            else if inp < #"a"
              then if inp = #"_"
                  then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                  else yyAction53(strm, yyNO_MATCH)
            else if inp = #"p"
              then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
            else if inp < #"p"
              then if inp = #"o"
                  then yyQ102(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                  else yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
              else yyAction53(strm, yyNO_MATCH)
      (* end case *))
fun yyQ100 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction10(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"A"
              then yyQ48(strm', yyMATCH(strm, yyAction10, yyNO_MATCH))
            else if inp < #"A"
              then if inp = #"("
                  then yyAction10(strm, yyNO_MATCH)
                else if inp < #"("
                  then if inp = #"'"
                      then yyQ48(strm', yyMATCH(strm, yyAction10, yyNO_MATCH))
                      else yyAction10(strm, yyNO_MATCH)
                else if inp = #"0"
                  then yyQ48(strm', yyMATCH(strm, yyAction10, yyNO_MATCH))
                else if inp < #"0"
                  then yyAction10(strm, yyNO_MATCH)
                else if inp <= #"9"
                  then yyQ48(strm', yyMATCH(strm, yyAction10, yyNO_MATCH))
                  else yyAction10(strm, yyNO_MATCH)
            else if inp = #"`"
              then yyAction10(strm, yyNO_MATCH)
            else if inp < #"`"
              then if inp = #"["
                  then yyAction10(strm, yyNO_MATCH)
                else if inp < #"["
                  then yyQ48(strm', yyMATCH(strm, yyAction10, yyNO_MATCH))
                else if inp = #"_"
                  then yyQ48(strm', yyMATCH(strm, yyAction10, yyNO_MATCH))
                  else yyAction10(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ48(strm', yyMATCH(strm, yyAction10, yyNO_MATCH))
              else yyAction10(strm, yyNO_MATCH)
      (* end case *))
fun yyQ99 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction6(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"A"
              then yyQ48(strm', yyMATCH(strm, yyAction6, yyNO_MATCH))
            else if inp < #"A"
              then if inp = #"("
                  then yyAction6(strm, yyNO_MATCH)
                else if inp < #"("
                  then if inp = #"'"
                      then yyQ48(strm', yyMATCH(strm, yyAction6, yyNO_MATCH))
                      else yyAction6(strm, yyNO_MATCH)
                else if inp = #"0"
                  then yyQ48(strm', yyMATCH(strm, yyAction6, yyNO_MATCH))
                else if inp < #"0"
                  then yyAction6(strm, yyNO_MATCH)
                else if inp <= #"9"
                  then yyQ48(strm', yyMATCH(strm, yyAction6, yyNO_MATCH))
                  else yyAction6(strm, yyNO_MATCH)
            else if inp = #"`"
              then yyAction6(strm, yyNO_MATCH)
            else if inp < #"`"
              then if inp = #"["
                  then yyAction6(strm, yyNO_MATCH)
                else if inp < #"["
                  then yyQ48(strm', yyMATCH(strm, yyAction6, yyNO_MATCH))
                else if inp = #"_"
                  then yyQ48(strm', yyMATCH(strm, yyAction6, yyNO_MATCH))
                  else yyAction6(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ48(strm', yyMATCH(strm, yyAction6, yyNO_MATCH))
              else yyAction6(strm, yyNO_MATCH)
      (* end case *))
fun yyQ30 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction53(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"`"
              then yyAction53(strm, yyNO_MATCH)
            else if inp < #"`"
              then if inp = #":"
                  then yyAction53(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp = #"("
                      then yyAction53(strm, yyNO_MATCH)
                    else if inp < #"("
                      then if inp = #"'"
                          then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                          else yyAction53(strm, yyNO_MATCH)
                    else if inp <= #"/"
                      then yyAction53(strm, yyNO_MATCH)
                      else yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                else if inp = #"["
                  then yyAction53(strm, yyNO_MATCH)
                else if inp < #"["
                  then if inp <= #"@"
                      then yyAction53(strm, yyNO_MATCH)
                      else yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                else if inp = #"_"
                  then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                  else yyAction53(strm, yyNO_MATCH)
            else if inp = #"j"
              then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
            else if inp < #"j"
              then if inp = #"g"
                  then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                else if inp < #"g"
                  then if inp = #"f"
                      then yyQ99(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                      else yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                else if inp = #"i"
                  then yyQ100(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                  else yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
            else if inp = #"s"
              then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
            else if inp < #"s"
              then if inp = #"r"
                  then yyQ101(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                  else yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
              else yyAction53(strm, yyNO_MATCH)
      (* end case *))
fun yyQ113 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction8(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"A"
              then yyQ48(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
            else if inp < #"A"
              then if inp = #"("
                  then yyAction8(strm, yyNO_MATCH)
                else if inp < #"("
                  then if inp = #"'"
                      then yyQ48(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
                      else yyAction8(strm, yyNO_MATCH)
                else if inp = #"0"
                  then yyQ48(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
                else if inp < #"0"
                  then yyAction8(strm, yyNO_MATCH)
                else if inp <= #"9"
                  then yyQ48(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
                  else yyAction8(strm, yyNO_MATCH)
            else if inp = #"`"
              then yyAction8(strm, yyNO_MATCH)
            else if inp < #"`"
              then if inp = #"["
                  then yyAction8(strm, yyNO_MATCH)
                else if inp < #"["
                  then yyQ48(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
                else if inp = #"_"
                  then yyQ48(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
                  else yyAction8(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ48(strm', yyMATCH(strm, yyAction8, yyNO_MATCH))
              else yyAction8(strm, yyNO_MATCH)
      (* end case *))
fun yyQ112 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction53(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction53(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #"0"
                  then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                else if inp < #"0"
                  then if inp = #"'"
                      then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                      else yyAction53(strm, yyNO_MATCH)
                else if inp = #":"
                  then yyAction53(strm, yyNO_MATCH)
                else if inp < #":"
                  then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction53(strm, yyNO_MATCH)
                  else yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
            else if inp = #"a"
              then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
            else if inp < #"a"
              then if inp = #"_"
                  then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                  else yyAction53(strm, yyNO_MATCH)
            else if inp = #"f"
              then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
            else if inp < #"f"
              then if inp = #"e"
                  then yyQ113(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                  else yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
              else yyAction53(strm, yyNO_MATCH)
      (* end case *))
fun yyQ111 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction53(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction53(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #"0"
                  then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                else if inp < #"0"
                  then if inp = #"'"
                      then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                      else yyAction53(strm, yyNO_MATCH)
                else if inp = #":"
                  then yyAction53(strm, yyNO_MATCH)
                else if inp < #":"
                  then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction53(strm, yyNO_MATCH)
                  else yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
            else if inp = #"a"
              then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
            else if inp < #"a"
              then if inp = #"_"
                  then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                  else yyAction53(strm, yyNO_MATCH)
            else if inp = #"t"
              then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
            else if inp < #"t"
              then if inp = #"s"
                  then yyQ112(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                  else yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
              else yyAction53(strm, yyNO_MATCH)
      (* end case *))
fun yyQ29 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction53(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction53(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #"0"
                  then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                else if inp < #"0"
                  then if inp = #"'"
                      then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                      else yyAction53(strm, yyNO_MATCH)
                else if inp = #":"
                  then yyAction53(strm, yyNO_MATCH)
                else if inp < #":"
                  then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction53(strm, yyNO_MATCH)
                  else yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
            else if inp = #"a"
              then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
            else if inp < #"a"
              then if inp = #"_"
                  then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                  else yyAction53(strm, yyNO_MATCH)
            else if inp = #"m"
              then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
            else if inp < #"m"
              then if inp = #"l"
                  then yyQ111(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                  else yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
              else yyAction53(strm, yyNO_MATCH)
      (* end case *))
fun yyQ114 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction13(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"A"
              then yyQ48(strm', yyMATCH(strm, yyAction13, yyNO_MATCH))
            else if inp < #"A"
              then if inp = #"("
                  then yyAction13(strm, yyNO_MATCH)
                else if inp < #"("
                  then if inp = #"'"
                      then yyQ48(strm', yyMATCH(strm, yyAction13, yyNO_MATCH))
                      else yyAction13(strm, yyNO_MATCH)
                else if inp = #"0"
                  then yyQ48(strm', yyMATCH(strm, yyAction13, yyNO_MATCH))
                else if inp < #"0"
                  then yyAction13(strm, yyNO_MATCH)
                else if inp <= #"9"
                  then yyQ48(strm', yyMATCH(strm, yyAction13, yyNO_MATCH))
                  else yyAction13(strm, yyNO_MATCH)
            else if inp = #"`"
              then yyAction13(strm, yyNO_MATCH)
            else if inp < #"`"
              then if inp = #"["
                  then yyAction13(strm, yyNO_MATCH)
                else if inp < #"["
                  then yyQ48(strm', yyMATCH(strm, yyAction13, yyNO_MATCH))
                else if inp = #"_"
                  then yyQ48(strm', yyMATCH(strm, yyAction13, yyNO_MATCH))
                  else yyAction13(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ48(strm', yyMATCH(strm, yyAction13, yyNO_MATCH))
              else yyAction13(strm, yyNO_MATCH)
      (* end case *))
fun yyQ28 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction53(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction53(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #"0"
                  then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                else if inp < #"0"
                  then if inp = #"'"
                      then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                      else yyAction53(strm, yyNO_MATCH)
                else if inp = #":"
                  then yyAction53(strm, yyNO_MATCH)
                else if inp < #":"
                  then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction53(strm, yyNO_MATCH)
                  else yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
            else if inp = #"a"
              then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
            else if inp < #"a"
              then if inp = #"_"
                  then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                  else yyAction53(strm, yyNO_MATCH)
            else if inp = #"p"
              then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
            else if inp < #"p"
              then if inp = #"o"
                  then yyQ114(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                  else yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
              else yyAction53(strm, yyNO_MATCH)
      (* end case *))
fun yyQ117 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction19(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"A"
              then yyQ48(strm', yyMATCH(strm, yyAction19, yyNO_MATCH))
            else if inp < #"A"
              then if inp = #"("
                  then yyAction19(strm, yyNO_MATCH)
                else if inp < #"("
                  then if inp = #"'"
                      then yyQ48(strm', yyMATCH(strm, yyAction19, yyNO_MATCH))
                      else yyAction19(strm, yyNO_MATCH)
                else if inp = #"0"
                  then yyQ48(strm', yyMATCH(strm, yyAction19, yyNO_MATCH))
                else if inp < #"0"
                  then yyAction19(strm, yyNO_MATCH)
                else if inp <= #"9"
                  then yyQ48(strm', yyMATCH(strm, yyAction19, yyNO_MATCH))
                  else yyAction19(strm, yyNO_MATCH)
            else if inp = #"`"
              then yyAction19(strm, yyNO_MATCH)
            else if inp < #"`"
              then if inp = #"["
                  then yyAction19(strm, yyNO_MATCH)
                else if inp < #"["
                  then yyQ48(strm', yyMATCH(strm, yyAction19, yyNO_MATCH))
                else if inp = #"_"
                  then yyQ48(strm', yyMATCH(strm, yyAction19, yyNO_MATCH))
                  else yyAction19(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ48(strm', yyMATCH(strm, yyAction19, yyNO_MATCH))
              else yyAction19(strm, yyNO_MATCH)
      (* end case *))
fun yyQ116 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction53(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction53(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #"0"
                  then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                else if inp < #"0"
                  then if inp = #"'"
                      then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                      else yyAction53(strm, yyNO_MATCH)
                else if inp = #":"
                  then yyAction53(strm, yyNO_MATCH)
                else if inp < #":"
                  then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction53(strm, yyNO_MATCH)
                  else yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
            else if inp = #"a"
              then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
            else if inp < #"a"
              then if inp = #"_"
                  then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                  else yyAction53(strm, yyNO_MATCH)
            else if inp = #"m"
              then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
            else if inp < #"m"
              then if inp = #"l"
                  then yyQ117(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                  else yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
              else yyAction53(strm, yyNO_MATCH)
      (* end case *))
fun yyQ115 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction53(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction53(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #"0"
                  then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                else if inp < #"0"
                  then if inp = #"'"
                      then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                      else yyAction53(strm, yyNO_MATCH)
                else if inp = #":"
                  then yyAction53(strm, yyNO_MATCH)
                else if inp < #":"
                  then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction53(strm, yyNO_MATCH)
                  else yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
            else if inp = #"a"
              then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
            else if inp < #"a"
              then if inp = #"_"
                  then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                  else yyAction53(strm, yyNO_MATCH)
            else if inp = #"m"
              then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
            else if inp < #"m"
              then if inp = #"l"
                  then yyQ116(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                  else yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
              else yyAction53(strm, yyNO_MATCH)
      (* end case *))
fun yyQ27 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction53(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction53(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #"0"
                  then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                else if inp < #"0"
                  then if inp = #"'"
                      then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                      else yyAction53(strm, yyNO_MATCH)
                else if inp = #":"
                  then yyAction53(strm, yyNO_MATCH)
                else if inp < #":"
                  then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction53(strm, yyNO_MATCH)
                  else yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
            else if inp = #"a"
              then yyQ115(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
            else if inp < #"a"
              then if inp = #"_"
                  then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                  else yyAction53(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
              else yyAction53(strm, yyNO_MATCH)
      (* end case *))
fun yyQ123 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction24(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"A"
              then yyQ48(strm', yyMATCH(strm, yyAction24, yyNO_MATCH))
            else if inp < #"A"
              then if inp = #"("
                  then yyAction24(strm, yyNO_MATCH)
                else if inp < #"("
                  then if inp = #"'"
                      then yyQ48(strm', yyMATCH(strm, yyAction24, yyNO_MATCH))
                      else yyAction24(strm, yyNO_MATCH)
                else if inp = #"0"
                  then yyQ48(strm', yyMATCH(strm, yyAction24, yyNO_MATCH))
                else if inp < #"0"
                  then yyAction24(strm, yyNO_MATCH)
                else if inp <= #"9"
                  then yyQ48(strm', yyMATCH(strm, yyAction24, yyNO_MATCH))
                  else yyAction24(strm, yyNO_MATCH)
            else if inp = #"`"
              then yyAction24(strm, yyNO_MATCH)
            else if inp < #"`"
              then if inp = #"["
                  then yyAction24(strm, yyNO_MATCH)
                else if inp < #"["
                  then yyQ48(strm', yyMATCH(strm, yyAction24, yyNO_MATCH))
                else if inp = #"_"
                  then yyQ48(strm', yyMATCH(strm, yyAction24, yyNO_MATCH))
                  else yyAction24(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ48(strm', yyMATCH(strm, yyAction24, yyNO_MATCH))
              else yyAction24(strm, yyNO_MATCH)
      (* end case *))
fun yyQ122 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction53(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction53(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #"0"
                  then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                else if inp < #"0"
                  then if inp = #"'"
                      then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                      else yyAction53(strm, yyNO_MATCH)
                else if inp = #":"
                  then yyAction53(strm, yyNO_MATCH)
                else if inp < #":"
                  then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction53(strm, yyNO_MATCH)
                  else yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
            else if inp = #"a"
              then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
            else if inp < #"a"
              then if inp = #"_"
                  then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                  else yyAction53(strm, yyNO_MATCH)
            else if inp = #"o"
              then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
            else if inp < #"o"
              then if inp = #"n"
                  then yyQ123(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                  else yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
              else yyAction53(strm, yyNO_MATCH)
      (* end case *))
fun yyQ121 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction53(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction53(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #"0"
                  then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                else if inp < #"0"
                  then if inp = #"'"
                      then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                      else yyAction53(strm, yyNO_MATCH)
                else if inp = #":"
                  then yyAction53(strm, yyNO_MATCH)
                else if inp < #":"
                  then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction53(strm, yyNO_MATCH)
                  else yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
            else if inp = #"a"
              then yyQ122(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
            else if inp < #"a"
              then if inp = #"_"
                  then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                  else yyAction53(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
              else yyAction53(strm, yyNO_MATCH)
      (* end case *))
fun yyQ120 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction53(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction53(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #"0"
                  then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                else if inp < #"0"
                  then if inp = #"'"
                      then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                      else yyAction53(strm, yyNO_MATCH)
                else if inp = #":"
                  then yyAction53(strm, yyNO_MATCH)
                else if inp < #":"
                  then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction53(strm, yyNO_MATCH)
                  else yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
            else if inp = #"a"
              then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
            else if inp < #"a"
              then if inp = #"_"
                  then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                  else yyAction53(strm, yyNO_MATCH)
            else if inp = #"f"
              then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
            else if inp < #"f"
              then if inp = #"e"
                  then yyQ121(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                  else yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
              else yyAction53(strm, yyNO_MATCH)
      (* end case *))
fun yyQ119 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction53(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction53(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #"0"
                  then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                else if inp < #"0"
                  then if inp = #"'"
                      then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                      else yyAction53(strm, yyNO_MATCH)
                else if inp = #":"
                  then yyAction53(strm, yyNO_MATCH)
                else if inp < #":"
                  then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction53(strm, yyNO_MATCH)
                  else yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
            else if inp = #"a"
              then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
            else if inp < #"a"
              then if inp = #"_"
                  then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                  else yyAction53(strm, yyNO_MATCH)
            else if inp = #"m"
              then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
            else if inp < #"m"
              then if inp = #"l"
                  then yyQ120(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                  else yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
              else yyAction53(strm, yyNO_MATCH)
      (* end case *))
fun yyQ118 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction53(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction53(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #"0"
                  then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                else if inp < #"0"
                  then if inp = #"'"
                      then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                      else yyAction53(strm, yyNO_MATCH)
                else if inp = #":"
                  then yyAction53(strm, yyNO_MATCH)
                else if inp < #":"
                  then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction53(strm, yyNO_MATCH)
                  else yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
            else if inp = #"a"
              then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
            else if inp < #"a"
              then if inp = #"_"
                  then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                  else yyAction53(strm, yyNO_MATCH)
            else if inp = #"p"
              then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
            else if inp < #"p"
              then if inp = #"o"
                  then yyQ119(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                  else yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
              else yyAction53(strm, yyNO_MATCH)
      (* end case *))
fun yyQ26 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction53(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction53(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #"0"
                  then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                else if inp < #"0"
                  then if inp = #"'"
                      then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                      else yyAction53(strm, yyNO_MATCH)
                else if inp = #":"
                  then yyAction53(strm, yyNO_MATCH)
                else if inp < #":"
                  then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction53(strm, yyNO_MATCH)
                  else yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
            else if inp = #"a"
              then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
            else if inp < #"a"
              then if inp = #"_"
                  then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                  else yyAction53(strm, yyNO_MATCH)
            else if inp = #"p"
              then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
            else if inp < #"p"
              then if inp = #"o"
                  then yyQ118(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                  else yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
              else yyAction53(strm, yyNO_MATCH)
      (* end case *))
fun yyQ125 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction15(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"A"
              then yyQ48(strm', yyMATCH(strm, yyAction15, yyNO_MATCH))
            else if inp < #"A"
              then if inp = #"("
                  then yyAction15(strm, yyNO_MATCH)
                else if inp < #"("
                  then if inp = #"'"
                      then yyQ48(strm', yyMATCH(strm, yyAction15, yyNO_MATCH))
                      else yyAction15(strm, yyNO_MATCH)
                else if inp = #"0"
                  then yyQ48(strm', yyMATCH(strm, yyAction15, yyNO_MATCH))
                else if inp < #"0"
                  then yyAction15(strm, yyNO_MATCH)
                else if inp <= #"9"
                  then yyQ48(strm', yyMATCH(strm, yyAction15, yyNO_MATCH))
                  else yyAction15(strm, yyNO_MATCH)
            else if inp = #"`"
              then yyAction15(strm, yyNO_MATCH)
            else if inp < #"`"
              then if inp = #"["
                  then yyAction15(strm, yyNO_MATCH)
                else if inp < #"["
                  then yyQ48(strm', yyMATCH(strm, yyAction15, yyNO_MATCH))
                else if inp = #"_"
                  then yyQ48(strm', yyMATCH(strm, yyAction15, yyNO_MATCH))
                  else yyAction15(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ48(strm', yyMATCH(strm, yyAction15, yyNO_MATCH))
              else yyAction15(strm, yyNO_MATCH)
      (* end case *))
fun yyQ124 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction53(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction53(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #"0"
                  then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                else if inp < #"0"
                  then if inp = #"'"
                      then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                      else yyAction53(strm, yyNO_MATCH)
                else if inp = #":"
                  then yyAction53(strm, yyNO_MATCH)
                else if inp < #":"
                  then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction53(strm, yyNO_MATCH)
                  else yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
            else if inp = #"a"
              then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
            else if inp < #"a"
              then if inp = #"_"
                  then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                  else yyAction53(strm, yyNO_MATCH)
            else if inp = #"e"
              then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
            else if inp < #"e"
              then if inp = #"d"
                  then yyQ125(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                  else yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
              else yyAction53(strm, yyNO_MATCH)
      (* end case *))
fun yyQ25 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction53(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction53(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #"0"
                  then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                else if inp < #"0"
                  then if inp = #"'"
                      then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                      else yyAction53(strm, yyNO_MATCH)
                else if inp = #":"
                  then yyAction53(strm, yyNO_MATCH)
                else if inp < #":"
                  then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction53(strm, yyNO_MATCH)
                  else yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
            else if inp = #"a"
              then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
            else if inp < #"a"
              then if inp = #"_"
                  then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                  else yyAction53(strm, yyNO_MATCH)
            else if inp = #"o"
              then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
            else if inp < #"o"
              then if inp = #"n"
                  then yyQ124(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                  else yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
              else yyAction53(strm, yyNO_MATCH)
      (* end case *))
fun yyQ24 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction53(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"A"
              then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
            else if inp < #"A"
              then if inp = #"("
                  then yyAction53(strm, yyNO_MATCH)
                else if inp < #"("
                  then if inp = #"'"
                      then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                      else yyAction53(strm, yyNO_MATCH)
                else if inp = #"0"
                  then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                else if inp < #"0"
                  then yyAction53(strm, yyNO_MATCH)
                else if inp <= #"9"
                  then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                  else yyAction53(strm, yyNO_MATCH)
            else if inp = #"`"
              then yyAction53(strm, yyNO_MATCH)
            else if inp < #"`"
              then if inp = #"["
                  then yyAction53(strm, yyNO_MATCH)
                else if inp < #"["
                  then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                else if inp = #"_"
                  then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
                  else yyAction53(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ48(strm', yyMATCH(strm, yyAction53, yyNO_MATCH))
              else yyAction53(strm, yyNO_MATCH)
      (* end case *))
fun yyQ126 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction51(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction51(strm, yyNO_MATCH)
      (* end case *))
fun yyQ23 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction50(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"="
              then yyQ126(strm', yyMATCH(strm, yyAction50, yyNO_MATCH))
              else yyAction50(strm, yyNO_MATCH)
      (* end case *))
fun yyQ22 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction46(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction46(strm, yyNO_MATCH)
      (* end case *))
fun yyQ128 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction47(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction47(strm, yyNO_MATCH)
      (* end case *))
fun yyQ127 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction49(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction49(strm, yyNO_MATCH)
      (* end case *))
fun yyQ21 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction48(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #">"
              then yyQ128(strm', yyMATCH(strm, yyAction48, yyNO_MATCH))
            else if inp < #">"
              then if inp = #"="
                  then yyQ127(strm', yyMATCH(strm, yyAction48, yyNO_MATCH))
                  else yyAction48(strm, yyNO_MATCH)
              else yyAction48(strm, yyNO_MATCH)
      (* end case *))
fun yyQ20 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction26(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction26(strm, yyNO_MATCH)
      (* end case *))
fun yyQ129 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction32(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction32(strm, yyNO_MATCH)
      (* end case *))
fun yyQ19 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction55(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"="
              then yyQ129(strm', yyMATCH(strm, yyAction55, yyNO_MATCH))
              else yyAction55(strm, yyNO_MATCH)
      (* end case *))
fun yyQ18 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction4(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"/"
              then yyAction4(strm, yyNO_MATCH)
            else if inp < #"/"
              then if inp = #"."
                  then yyQ42(strm', yyMATCH(strm, yyAction4, yyNO_MATCH))
                  else yyAction4(strm, yyNO_MATCH)
            else if inp <= #"9"
              then yyQ43(strm', yyMATCH(strm, yyAction4, yyNO_MATCH))
              else yyAction4(strm, yyNO_MATCH)
      (* end case *))
fun yyQ17 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction42(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction42(strm, yyNO_MATCH)
      (* end case *))
fun yyQ134 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction38(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction38(strm, yyNO_MATCH)
      (* end case *))
fun yyQ133 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"."
              then yyQ134(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ135 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction36(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction36(strm, yyNO_MATCH)
      (* end case *))
fun yyQ132 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"."
              then yyQ135(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ136 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction35(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction35(strm, yyNO_MATCH)
      (* end case *))
fun yyQ131 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"."
              then yyQ136(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ137 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction37(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction37(strm, yyNO_MATCH)
      (* end case *))
fun yyQ130 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"."
              then yyQ137(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ16 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction55(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #","
              then yyAction55(strm, yyNO_MATCH)
            else if inp < #","
              then if inp = #")"
                  then yyAction55(strm, yyNO_MATCH)
                else if inp < #")"
                  then if inp = #"("
                      then yyQ44(strm', yyMATCH(strm, yyAction55, yyNO_MATCH))
                      else yyAction55(strm, yyNO_MATCH)
                else if inp = #"*"
                  then yyQ130(strm', yyMATCH(strm, yyAction55, yyNO_MATCH))
                  else yyQ131(strm', yyMATCH(strm, yyAction55, yyNO_MATCH))
            else if inp = #"/"
              then yyQ133(strm', yyMATCH(strm, yyAction55, yyNO_MATCH))
            else if inp < #"/"
              then if inp = #"-"
                  then yyQ132(strm', yyMATCH(strm, yyAction55, yyNO_MATCH))
                  else yyAction55(strm, yyNO_MATCH)
            else if inp <= #"9"
              then yyQ42(strm', yyMATCH(strm, yyAction55, yyNO_MATCH))
              else yyAction55(strm, yyNO_MATCH)
      (* end case *))
fun yyQ15 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction40(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction40(strm, yyNO_MATCH)
      (* end case *))
fun yyQ14 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction27(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction27(strm, yyNO_MATCH)
      (* end case *))
fun yyQ13 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction39(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction39(strm, yyNO_MATCH)
      (* end case *))
fun yyQ12 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction41(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction41(strm, yyNO_MATCH)
      (* end case *))
fun yyQ11 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction29(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction29(strm, yyNO_MATCH)
      (* end case *))
fun yyQ138 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction54(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction54(strm, yyNO_MATCH)
      (* end case *))
fun yyQ10 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction28(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"*"
              then yyQ138(strm', yyMATCH(strm, yyAction28, yyNO_MATCH))
              else yyAction28(strm, yyNO_MATCH)
      (* end case *))
fun yyQ139 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction44(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction44(strm, yyNO_MATCH)
      (* end case *))
fun yyQ9 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction55(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"&"
              then yyQ139(strm', yyMATCH(strm, yyAction55, yyNO_MATCH))
              else yyAction55(strm, yyNO_MATCH)
      (* end case *))
fun yyQ8 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction43(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction43(strm, yyNO_MATCH)
      (* end case *))
fun yyQ7 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction52(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction52(strm, yyNO_MATCH)
      (* end case *))
fun yyQ5 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction2(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction2(strm, yyNO_MATCH)
      (* end case *))
fun yyQ6 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction2(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"\n"
              then yyQ5(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
              else yyAction2(strm, yyNO_MATCH)
      (* end case *))
fun yyQ140 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction1(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"\n"
              then yyAction1(strm, yyNO_MATCH)
            else if inp < #"\n"
              then if inp = #"\t"
                  then yyQ140(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
                  else yyAction1(strm, yyNO_MATCH)
            else if inp = #" "
              then yyQ140(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
              else yyAction1(strm, yyNO_MATCH)
      (* end case *))
fun yyQ4 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction1(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"\n"
              then yyAction1(strm, yyNO_MATCH)
            else if inp < #"\n"
              then if inp = #"\t"
                  then yyQ140(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
                  else yyAction1(strm, yyNO_MATCH)
            else if inp = #" "
              then yyQ140(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
              else yyAction1(strm, yyNO_MATCH)
      (* end case *))
fun yyQ3 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction55(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction55(strm, yyNO_MATCH)
      (* end case *))
fun yyQ0 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE =>
            if yyInput.eof(!(yystrm))
              then UserDeclarations.eof(yyarg)
              else yyAction1(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"A"
              then yyQ24(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
            else if inp < #"A"
              then if inp = #")"
                  then yyQ11(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
                else if inp < #")"
                  then if inp = #" "
                      then yyQ4(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
                    else if inp < #" "
                      then if inp = #"\v"
                          then yyQ3(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
                        else if inp < #"\v"
                          then if inp = #"\t"
                              then yyQ4(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
                            else if inp = #"\n"
                              then yyQ5(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
                              else yyQ3(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
                        else if inp = #"\r"
                          then yyQ6(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
                          else yyQ3(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
                    else if inp = #"&"
                      then yyQ9(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
                    else if inp < #"&"
                      then if inp = #"\""
                          then yyQ3(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
                        else if inp < #"\""
                          then yyQ7(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
                        else if inp = #"%"
                          then yyQ8(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
                          else yyQ3(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
                    else if inp = #"'"
                      then yyQ3(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
                      else yyQ10(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
                else if inp = #"0"
                  then yyQ18(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
                else if inp < #"0"
                  then if inp = #"-"
                      then yyQ15(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
                    else if inp < #"-"
                      then if inp = #"+"
                          then yyQ13(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
                        else if inp = #"*"
                          then yyQ12(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
                          else yyQ14(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
                    else if inp = #"."
                      then yyQ16(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
                      else yyQ17(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
                else if inp = #"<"
                  then yyQ21(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
                else if inp < #"<"
                  then if inp = #":"
                      then yyQ19(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
                    else if inp = #";"
                      then yyQ20(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
                      else yyQ18(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
                else if inp = #">"
                  then yyQ23(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
                else if inp = #"="
                  then yyQ22(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
                  else yyQ3(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
            else if inp = #"o"
              then yyQ33(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
            else if inp < #"o"
              then if inp = #"e"
                  then yyQ29(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
                else if inp < #"e"
                  then if inp = #"b"
                      then yyQ26(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
                    else if inp < #"b"
                      then if inp = #"["
                          then yyQ3(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
                        else if inp < #"["
                          then yyQ24(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
                        else if inp = #"a"
                          then yyQ25(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
                          else yyQ3(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
                    else if inp = #"c"
                      then yyQ27(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
                      else yyQ28(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
                else if inp = #"j"
                  then yyQ24(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
                else if inp < #"j"
                  then if inp = #"g"
                      then yyQ24(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
                    else if inp < #"g"
                      then yyQ30(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
                    else if inp = #"i"
                      then yyQ31(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
                      else yyQ24(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
                else if inp = #"m"
                  then yyQ32(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
                  else yyQ24(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
            else if inp = #"w"
              then yyQ37(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
            else if inp < #"w"
              then if inp = #"s"
                  then yyQ24(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
                else if inp < #"s"
                  then if inp = #"q"
                      then yyQ24(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
                    else if inp = #"p"
                      then yyQ34(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
                      else yyQ35(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
                else if inp = #"t"
                  then yyQ36(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
                  else yyQ24(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
            else if inp = #"}"
              then yyQ40(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
            else if inp < #"}"
              then if inp = #"{"
                  then yyQ38(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
                else if inp = #"|"
                  then yyQ39(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
                  else yyQ24(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
            else if inp = #"~"
              then yyQ41(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
              else yyQ3(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
      (* end case *))
in
  (case (!(yyss))
   of PI => yyQ0(!(yystrm), yyNO_MATCH)
    | COMMENT => yyQ1(!(yystrm), yyNO_MATCH)
    | INITIAL => yyQ2(!(yystrm), yyNO_MATCH)
  (* end case *))
end
            end
	  in 
            continue() 	  
	    handle IO.Io{cause, ...} => raise cause
          end
        in 
          lex 
        end
    in
    fun makeLexer yyinputN = mk (yyInput.mkStream yyinputN)
    fun makeLexer' ins = mk (yyInput.mkStream ins)
    end

  end
