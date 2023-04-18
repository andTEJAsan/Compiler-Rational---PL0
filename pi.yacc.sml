functor PiLrValsFun(structure Token : TOKEN)
 : sig structure ParserData : PARSER_DATA
       structure Tokens : Pi_TOKENS
   end
 = 
struct
structure ParserData=
struct
structure Header = 
struct
open DataTypes

end
structure LrTable = Token.LrTable
structure Token = Token
local open LrTable in 
val table=let val actionRows =
"\
\\001\000\001\000\007\000\002\000\108\000\003\000\108\000\006\000\105\000\000\000\
\\001\000\004\000\012\000\000\000\
\\001\000\004\000\015\000\000\000\
\\001\000\004\000\025\000\007\000\024\000\036\000\023\000\037\000\022\000\
\\041\000\021\000\000\000\
\\001\000\004\000\028\000\000\000\
\\001\000\004\000\042\000\000\000\
\\001\000\005\000\030\000\000\000\
\\001\000\005\000\041\000\000\000\
\\001\000\005\000\044\000\000\000\
\\001\000\005\000\072\000\000\000\
\\001\000\005\000\074\000\000\000\
\\001\000\006\000\011\000\000\000\
\\001\000\007\000\073\000\000\000\
\\001\000\008\000\037\000\013\000\036\000\014\000\035\000\033\000\034\000\
\\034\000\033\000\035\000\032\000\000\000\
\\001\000\008\000\039\000\000\000\
\\001\000\009\000\093\000\015\000\064\000\016\000\063\000\017\000\062\000\
\\018\000\061\000\019\000\060\000\020\000\059\000\021\000\058\000\
\\022\000\057\000\023\000\056\000\025\000\055\000\026\000\054\000\
\\027\000\053\000\028\000\052\000\029\000\051\000\030\000\050\000\
\\031\000\049\000\032\000\048\000\000\000\
\\001\000\009\000\095\000\015\000\064\000\016\000\063\000\017\000\062\000\
\\018\000\061\000\019\000\060\000\020\000\059\000\021\000\058\000\
\\022\000\057\000\023\000\056\000\025\000\055\000\026\000\054\000\
\\027\000\053\000\028\000\052\000\029\000\051\000\030\000\050\000\
\\031\000\049\000\032\000\048\000\000\000\
\\001\000\011\000\000\000\000\000\
\\001\000\012\000\040\000\000\000\
\\001\000\015\000\064\000\016\000\063\000\017\000\062\000\018\000\061\000\
\\019\000\060\000\020\000\059\000\021\000\058\000\022\000\057\000\
\\023\000\056\000\025\000\055\000\026\000\054\000\027\000\053\000\
\\028\000\052\000\029\000\051\000\030\000\050\000\031\000\049\000\
\\032\000\048\000\038\000\068\000\000\000\
\\001\000\015\000\064\000\016\000\063\000\017\000\062\000\018\000\061\000\
\\019\000\060\000\020\000\059\000\021\000\058\000\022\000\057\000\
\\023\000\056\000\025\000\055\000\026\000\054\000\027\000\053\000\
\\028\000\052\000\029\000\051\000\030\000\050\000\031\000\049\000\
\\032\000\048\000\043\000\047\000\000\000\
\\001\000\039\000\098\000\000\000\
\\001\000\040\000\100\000\000\000\
\\001\000\042\000\097\000\000\000\
\\102\000\000\000\
\\103\000\000\000\
\\104\000\000\000\
\\106\000\000\000\
\\107\000\000\000\
\\109\000\000\000\
\\110\000\002\000\009\000\000\000\
\\111\000\000\000\
\\112\000\003\000\014\000\000\000\
\\113\000\000\000\
\\114\000\010\000\027\000\000\000\
\\115\000\000\000\
\\116\000\000\000\
\\117\000\000\000\
\\118\000\004\000\025\000\036\000\023\000\037\000\022\000\041\000\021\000\000\000\
\\119\000\000\000\
\\120\000\000\000\
\\121\000\000\000\
\\122\000\000\000\
\\123\000\000\000\
\\124\000\000\000\
\\125\000\000\000\
\\126\000\015\000\064\000\016\000\063\000\017\000\062\000\018\000\061\000\
\\019\000\060\000\020\000\059\000\021\000\058\000\022\000\057\000\
\\023\000\056\000\025\000\055\000\026\000\054\000\027\000\053\000\
\\028\000\052\000\029\000\051\000\030\000\050\000\031\000\049\000\
\\032\000\048\000\000\000\
\\127\000\000\000\
\\128\000\000\000\
\\129\000\017\000\062\000\018\000\061\000\025\000\055\000\026\000\054\000\
\\027\000\053\000\028\000\052\000\029\000\051\000\030\000\050\000\
\\031\000\049\000\032\000\048\000\000\000\
\\130\000\017\000\062\000\018\000\061\000\025\000\055\000\026\000\054\000\
\\027\000\053\000\028\000\052\000\029\000\051\000\030\000\050\000\
\\031\000\049\000\032\000\048\000\000\000\
\\131\000\025\000\055\000\026\000\054\000\027\000\053\000\028\000\052\000\
\\029\000\051\000\030\000\050\000\031\000\049\000\032\000\048\000\000\000\
\\132\000\025\000\055\000\026\000\054\000\027\000\053\000\028\000\052\000\
\\029\000\051\000\030\000\050\000\031\000\049\000\032\000\048\000\000\000\
\\133\000\015\000\064\000\016\000\063\000\017\000\062\000\018\000\061\000\
\\021\000\058\000\022\000\057\000\023\000\056\000\025\000\055\000\
\\026\000\054\000\027\000\053\000\028\000\052\000\029\000\051\000\
\\030\000\050\000\031\000\049\000\032\000\048\000\000\000\
\\134\000\015\000\064\000\016\000\063\000\017\000\062\000\018\000\061\000\
\\021\000\058\000\022\000\057\000\023\000\056\000\025\000\055\000\
\\026\000\054\000\027\000\053\000\028\000\052\000\029\000\051\000\
\\030\000\050\000\031\000\049\000\032\000\048\000\000\000\
\\135\000\015\000\064\000\016\000\063\000\017\000\062\000\018\000\061\000\
\\025\000\055\000\026\000\054\000\027\000\053\000\028\000\052\000\
\\029\000\051\000\030\000\050\000\031\000\049\000\032\000\048\000\000\000\
\\136\000\015\000\064\000\016\000\063\000\017\000\062\000\018\000\061\000\
\\025\000\055\000\026\000\054\000\027\000\053\000\028\000\052\000\
\\029\000\051\000\030\000\050\000\031\000\049\000\032\000\048\000\000\000\
\\137\000\015\000\064\000\016\000\063\000\017\000\062\000\018\000\061\000\
\\025\000\055\000\026\000\054\000\027\000\053\000\028\000\052\000\
\\029\000\051\000\030\000\050\000\031\000\049\000\032\000\048\000\000\000\
\\138\000\027\000\053\000\028\000\052\000\029\000\051\000\030\000\050\000\
\\031\000\049\000\032\000\048\000\000\000\
\\139\000\027\000\053\000\028\000\052\000\029\000\051\000\030\000\050\000\
\\031\000\049\000\032\000\048\000\000\000\
\\140\000\000\000\
\\141\000\000\000\
\\142\000\000\000\
\\143\000\000\000\
\\144\000\000\000\
\\145\000\000\000\
\\146\000\000\000\
\\147\000\000\000\
\\148\000\000\000\
\\149\000\000\000\
\"
val actionRowNumbers =
"\000\000\030\000\026\000\011\000\
\\024\000\001\000\032\000\002\000\
\\025\000\003\000\034\000\027\000\
\\004\000\034\000\006\000\041\000\
\\042\000\040\000\039\000\013\000\
\\013\000\014\000\035\000\018\000\
\\007\000\005\000\034\000\008\000\
\\038\000\020\000\069\000\068\000\
\\067\000\013\000\013\000\013\000\
\\019\000\013\000\013\000\028\000\
\\034\000\009\000\029\000\012\000\
\\010\000\011\000\013\000\013\000\
\\013\000\013\000\013\000\013\000\
\\013\000\013\000\013\000\013\000\
\\013\000\013\000\013\000\013\000\
\\013\000\013\000\013\000\048\000\
\\047\000\015\000\011\000\016\000\
\\046\000\033\000\031\000\036\000\
\\038\000\023\000\065\000\064\000\
\\063\000\062\000\061\000\060\000\
\\059\000\058\000\057\000\056\000\
\\055\000\054\000\053\000\052\000\
\\051\000\050\000\049\000\066\000\
\\021\000\045\000\037\000\043\000\
\\011\000\022\000\044\000\017\000"
val gotoT =
"\
\\001\000\099\000\002\000\004\000\003\000\003\000\004\000\002\000\
\\005\000\001\000\000\000\
\\006\000\006\000\000\000\
\\000\000\
\\016\000\008\000\000\000\
\\000\000\
\\000\000\
\\007\000\011\000\000\000\
\\000\000\
\\000\000\
\\010\000\018\000\011\000\017\000\012\000\016\000\013\000\015\000\
\\014\000\014\000\000\000\
\\008\000\024\000\000\000\
\\000\000\
\\000\000\
\\008\000\027\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\009\000\029\000\000\000\
\\009\000\036\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\008\000\041\000\000\000\
\\000\000\
\\010\000\018\000\011\000\017\000\012\000\016\000\013\000\015\000\
\\014\000\044\000\015\000\043\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\009\000\063\000\000\000\
\\009\000\064\000\000\000\
\\009\000\065\000\000\000\
\\000\000\
\\009\000\067\000\000\000\
\\009\000\068\000\000\000\
\\000\000\
\\008\000\069\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\016\000\073\000\000\000\
\\009\000\074\000\000\000\
\\009\000\075\000\000\000\
\\009\000\076\000\000\000\
\\009\000\077\000\000\000\
\\009\000\078\000\000\000\
\\009\000\079\000\000\000\
\\009\000\080\000\000\000\
\\009\000\081\000\000\000\
\\009\000\082\000\000\000\
\\009\000\083\000\000\000\
\\009\000\084\000\000\000\
\\009\000\085\000\000\000\
\\009\000\086\000\000\000\
\\009\000\087\000\000\000\
\\009\000\088\000\000\000\
\\009\000\089\000\000\000\
\\009\000\090\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\016\000\092\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\010\000\018\000\011\000\017\000\012\000\016\000\013\000\015\000\
\\014\000\044\000\015\000\094\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\016\000\097\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\"
val numstates = 100
val numrules = 48
val s = ref "" and index = ref 0
val string_to_int = fn () => 
let val i = !index
in index := i+2; Char.ord(String.sub(!s,i)) + Char.ord(String.sub(!s,i+1)) * 256
end
val string_to_list = fn s' =>
    let val len = String.size s'
        fun f () =
           if !index < len then string_to_int() :: f()
           else nil
   in index := 0; s := s'; f ()
   end
val string_to_pairlist = fn (conv_key,conv_entry) =>
     let fun f () =
         case string_to_int()
         of 0 => EMPTY
          | n => PAIR(conv_key (n-1),conv_entry (string_to_int()),f())
     in f
     end
val string_to_pairlist_default = fn (conv_key,conv_entry) =>
    let val conv_row = string_to_pairlist(conv_key,conv_entry)
    in fn () =>
       let val default = conv_entry(string_to_int())
           val row = conv_row()
       in (row,default)
       end
   end
val string_to_table = fn (convert_row,s') =>
    let val len = String.size s'
        fun f ()=
           if !index < len then convert_row() :: f()
           else nil
     in (s := s'; index := 0; f ())
     end
local
  val memo = Array.array(numstates+numrules,ERROR)
  val _ =let fun g i=(Array.update(memo,i,REDUCE(i-numstates)); g(i+1))
       fun f i =
            if i=numstates then g i
            else (Array.update(memo,i,SHIFT (STATE i)); f (i+1))
          in f 0 handle General.Subscript => ()
          end
in
val entry_to_action = fn 0 => ACCEPT | 1 => ERROR | j => Array.sub(memo,(j-2))
end
val gotoT=Array.fromList(string_to_table(string_to_pairlist(NT,STATE),gotoT))
val actionRows=string_to_table(string_to_pairlist_default(T,entry_to_action),actionRows)
val actionRowNumbers = string_to_list actionRowNumbers
val actionT = let val actionRowLookUp=
let val a=Array.fromList(actionRows) in fn i=>Array.sub(a,i) end
in Array.fromList(List.map actionRowLookUp actionRowNumbers)
end
in LrTable.mkLrTable {actions=actionT,gotos=gotoT,numRules=numrules,
numStates=numstates,initialState=STATE 0}
end
end
local open Header in
type pos = int
type arg = string
structure MlyValue = 
struct
datatype svalue = VOID | ntVOID of unit ->  unit
 | INTNUM of unit ->  (BigInt.bigint) | BOOLNUM of unit ->  (bool)
 | RATNUM of unit ->  (Rational.rational) | IDEN of unit ->  (string)
 | commandseq of unit ->  (Cmd list) | res of unit ->  (Cmd list)
 | command of unit ->  (Cmd) | conditionalcmd of unit ->  (Cmd)
 | whilecmd of unit ->  (Cmd) | printcmd of unit ->  (Cmd)
 | assignmentcmd of unit ->  (Cmd)
 | expression of unit ->  (Expression) | rep of unit ->  (string list)
 | boolvardecls of unit ->  ( ( int_rat_bool_decls list ) )
 | intvardecls of unit ->  ( ( int_rat_bool_decls list ) )
 | ratvardecls of unit ->  ( ( int_rat_bool_decls list ) )
 | vardecls of unit ->  ( ( int_rat_bool_decls list ) * ( int_rat_bool_decls list ) * ( int_rat_bool_decls list ) )
 | declseq of unit ->  ( ( int_rat_bool_decls list ) * ( int_rat_bool_decls list ) * ( int_rat_bool_decls list ) )
 | block of unit ->  (blockans) | program of unit ->  (blockans)
end
type svalue = MlyValue.svalue
type result = blockans
end
structure EC=
struct
open LrTable
infix 5 $$
fun x $$ y = y::x
val is_keyword =
fn (T 0) => true | (T 2) => true | (T 1) => true | (T 40) => true | 
(T 42) => true | (T 41) => true | (T 36) => true | (T 39) => true | 
(T 37) => true | (T 38) => true | (T 35) => true | _ => false
val preferred_change : (term list * term list) list = 
nil
val noShift = 
fn (T 10) => true | _ => false
val showTerminal =
fn (T 0) => "RATDL"
  | (T 1) => "INTDL"
  | (T 2) => "BOOLDL"
  | (T 3) => "IDEN"
  | (T 4) => "SEMI"
  | (T 5) => "LBRACE"
  | (T 6) => "RBRACE"
  | (T 7) => "LPAREN"
  | (T 8) => "RPAREN"
  | (T 9) => "COMMA"
  | (T 10) => "EOF"
  | (T 11) => "ASSIGN"
  | (T 12) => "NEG"
  | (T 13) => "INV"
  | (T 14) => "RATPLUS"
  | (T 15) => "RATSUB"
  | (T 16) => "RATMUL"
  | (T 17) => "RATDIV"
  | (T 18) => "PLUS"
  | (T 19) => "SUB"
  | (T 20) => "MUL"
  | (T 21) => "DIV"
  | (T 22) => "MOD"
  | (T 23) => "NOT"
  | (T 24) => "AND"
  | (T 25) => "OR"
  | (T 26) => "EQ"
  | (T 27) => "NE"
  | (T 28) => "LT"
  | (T 29) => "LE"
  | (T 30) => "GT"
  | (T 31) => "GE"
  | (T 32) => "RATNUM"
  | (T 33) => "BOOLNUM"
  | (T 34) => "INTNUM"
  | (T 35) => "PRINT"
  | (T 36) => "IF"
  | (T 37) => "THEN"
  | (T 38) => "ELSE"
  | (T 39) => "FI"
  | (T 40) => "WHILE"
  | (T 41) => "OD"
  | (T 42) => "DO"
  | _ => "bogus-term"
local open Header in
val errtermvalue=
fn _ => MlyValue.VOID
end
val terms : term list = nil
 $$ (T 42) $$ (T 41) $$ (T 40) $$ (T 39) $$ (T 38) $$ (T 37) $$ (T 36)
 $$ (T 35) $$ (T 31) $$ (T 30) $$ (T 29) $$ (T 28) $$ (T 27) $$ (T 26)
 $$ (T 25) $$ (T 24) $$ (T 23) $$ (T 22) $$ (T 21) $$ (T 20) $$ (T 19)
 $$ (T 18) $$ (T 17) $$ (T 16) $$ (T 15) $$ (T 14) $$ (T 13) $$ (T 12)
 $$ (T 11) $$ (T 10) $$ (T 9) $$ (T 8) $$ (T 7) $$ (T 6) $$ (T 5) $$ 
(T 4) $$ (T 2) $$ (T 1) $$ (T 0)end
structure Actions =
struct 
exception mlyAction of int
local open Header in
val actions = 
fn (i392,defaultPos,stack,
    (fileName):arg) =>
case (i392,stack)
of  ( 0, ( ( _, ( MlyValue.block block1, block1left, block1right)) :: 
rest671)) => let val  result = MlyValue.program (fn _ => let val  (
block as block1) = block1 ()
 in (block)
end)
 in ( LrTable.NT 0, ( result, block1left, block1right), rest671)
end
|  ( 1, ( ( _, ( MlyValue.commandseq commandseq1, _, commandseq1right)
) :: ( _, ( MlyValue.declseq declseq1, declseq1left, _)) :: rest671))
 => let val  result = MlyValue.block (fn _ => let val  (declseq as 
declseq1) = declseq1 ()
 val  (commandseq as commandseq1) = commandseq1 ()
 in (blockans(declseq,commandseq))
end)
 in ( LrTable.NT 1, ( result, declseq1left, commandseq1right), rest671
)
end
|  ( 2, ( ( _, ( MlyValue.vardecls vardecls1, vardecls1left, 
vardecls1right)) :: rest671)) => let val  result = MlyValue.declseq
 (fn _ => let val  (vardecls as vardecls1) = vardecls1 ()
 in (vardecls)
end)
 in ( LrTable.NT 2, ( result, vardecls1left, vardecls1right), rest671)

end
|  ( 3, ( rest671)) => let val  result = MlyValue.declseq (fn _ => (
([],[],[])))
 in ( LrTable.NT 2, ( result, defaultPos, defaultPos), rest671)
end
|  ( 4, ( ( _, ( MlyValue.boolvardecls boolvardecls1, _, 
boolvardecls1right)) :: ( _, ( MlyValue.intvardecls intvardecls1, _, _
)) :: ( _, ( MlyValue.ratvardecls ratvardecls1, ratvardecls1left, _))
 :: rest671)) => let val  result = MlyValue.vardecls (fn _ => let val 
 (ratvardecls as ratvardecls1) = ratvardecls1 ()
 val  (intvardecls as intvardecls1) = intvardecls1 ()
 val  (boolvardecls as boolvardecls1) = boolvardecls1 ()
 in ((ratvardecls,intvardecls,boolvardecls))
end)
 in ( LrTable.NT 3, ( result, ratvardecls1left, boolvardecls1right), 
rest671)
end
|  ( 5, ( ( _, ( _, _, SEMI1right)) :: ( _, ( MlyValue.rep rep1, _, _)
) :: ( _, ( MlyValue.IDEN IDEN1, _, _)) :: ( _, ( _, RATDL1left, _))
 :: rest671)) => let val  result = MlyValue.ratvardecls (fn _ => let
 val  (IDEN as IDEN1) = IDEN1 ()
 val  (rep as rep1) = rep1 ()
 in (map RATa (IDEN::rep))
end)
 in ( LrTable.NT 4, ( result, RATDL1left, SEMI1right), rest671)
end
|  ( 6, ( rest671)) => let val  result = MlyValue.ratvardecls (fn _ =>
 ([]))
 in ( LrTable.NT 4, ( result, defaultPos, defaultPos), rest671)
end
|  ( 7, ( ( _, ( _, _, SEMI1right)) :: ( _, ( MlyValue.rep rep1, _, _)
) :: ( _, ( MlyValue.IDEN IDEN1, _, _)) :: ( _, ( _, INTDL1left, _))
 :: rest671)) => let val  result = MlyValue.intvardecls (fn _ => let
 val  (IDEN as IDEN1) = IDEN1 ()
 val  (rep as rep1) = rep1 ()
 in (map INTa (IDEN::rep))
end)
 in ( LrTable.NT 5, ( result, INTDL1left, SEMI1right), rest671)
end
|  ( 8, ( rest671)) => let val  result = MlyValue.intvardecls (fn _ =>
 ([]))
 in ( LrTable.NT 5, ( result, defaultPos, defaultPos), rest671)
end
|  ( 9, ( ( _, ( _, _, SEMI1right)) :: ( _, ( MlyValue.rep rep1, _, _)
) :: ( _, ( MlyValue.IDEN IDEN1, _, _)) :: ( _, ( _, BOOLDL1left, _))
 :: rest671)) => let val  result = MlyValue.boolvardecls (fn _ => let
 val  (IDEN as IDEN1) = IDEN1 ()
 val  (rep as rep1) = rep1 ()
 in (map BOOLa (IDEN::rep))
end)
 in ( LrTable.NT 6, ( result, BOOLDL1left, SEMI1right), rest671)
end
|  ( 10, ( rest671)) => let val  result = MlyValue.boolvardecls (fn _
 => ([]))
 in ( LrTable.NT 6, ( result, defaultPos, defaultPos), rest671)
end
|  ( 11, ( ( _, ( MlyValue.rep rep1, _, rep1right)) :: ( _, ( 
MlyValue.IDEN IDEN1, _, _)) :: ( _, ( _, COMMA1left, _)) :: rest671))
 => let val  result = MlyValue.rep (fn _ => let val  (IDEN as IDEN1) =
 IDEN1 ()
 val  (rep as rep1) = rep1 ()
 in (IDEN::rep)
end)
 in ( LrTable.NT 7, ( result, COMMA1left, rep1right), rest671)
end
|  ( 12, ( rest671)) => let val  result = MlyValue.rep (fn _ => ([]))
 in ( LrTable.NT 7, ( result, defaultPos, defaultPos), rest671)
end
|  ( 13, ( ( _, ( _, _, RBRACE1right)) :: ( _, ( _, LBRACE1left, _))
 :: rest671)) => let val  result = MlyValue.commandseq (fn _ => ([]))
 in ( LrTable.NT 15, ( result, LBRACE1left, RBRACE1right), rest671)

end
|  ( 14, ( ( _, ( _, _, RBRACE1right)) :: ( _, ( MlyValue.res res1, _,
 _)) :: _ :: ( _, ( MlyValue.command command1, _, _)) :: ( _, ( _, 
LBRACE1left, _)) :: rest671)) => let val  result = MlyValue.commandseq
 (fn _ => let val  (command as command1) = command1 ()
 val  (res as res1) = res1 ()
 in (command::res)
end)
 in ( LrTable.NT 15, ( result, LBRACE1left, RBRACE1right), rest671)

end
|  ( 15, ( ( _, ( MlyValue.res res1, _, res1right)) :: _ :: ( _, ( 
MlyValue.command command1, command1left, _)) :: rest671)) => let val  
result = MlyValue.res (fn _ => let val  (command as command1) = 
command1 ()
 val  (res as res1) = res1 ()
 in (command::res)
end)
 in ( LrTable.NT 14, ( result, command1left, res1right), rest671)
end
|  ( 16, ( rest671)) => let val  result = MlyValue.res (fn _ => ([]))
 in ( LrTable.NT 14, ( result, defaultPos, defaultPos), rest671)
end
|  ( 17, ( ( _, ( MlyValue.assignmentcmd assignmentcmd1, 
assignmentcmd1left, assignmentcmd1right)) :: rest671)) => let val  
result = MlyValue.command (fn _ => let val  (assignmentcmd as 
assignmentcmd1) = assignmentcmd1 ()
 in (assignmentcmd)
end)
 in ( LrTable.NT 13, ( result, assignmentcmd1left, assignmentcmd1right
), rest671)
end
|  ( 18, ( ( _, ( MlyValue.printcmd printcmd1, printcmd1left, 
printcmd1right)) :: rest671)) => let val  result = MlyValue.command
 (fn _ => let val  (printcmd as printcmd1) = printcmd1 ()
 in (printcmd)
end)
 in ( LrTable.NT 13, ( result, printcmd1left, printcmd1right), rest671
)
end
|  ( 19, ( ( _, ( MlyValue.conditionalcmd conditionalcmd1, 
conditionalcmd1left, conditionalcmd1right)) :: rest671)) => let val  
result = MlyValue.command (fn _ => let val  (conditionalcmd as 
conditionalcmd1) = conditionalcmd1 ()
 in (conditionalcmd)
end)
 in ( LrTable.NT 13, ( result, conditionalcmd1left, 
conditionalcmd1right), rest671)
end
|  ( 20, ( ( _, ( MlyValue.whilecmd whilecmd1, whilecmd1left, 
whilecmd1right)) :: rest671)) => let val  result = MlyValue.command
 (fn _ => let val  (whilecmd as whilecmd1) = whilecmd1 ()
 in (whilecmd)
end)
 in ( LrTable.NT 13, ( result, whilecmd1left, whilecmd1right), rest671
)
end
|  ( 21, ( ( _, ( _, _, OD1right)) :: ( _, ( MlyValue.commandseq 
commandseq1, _, _)) :: _ :: ( _, ( MlyValue.expression expression1, _,
 _)) :: ( _, ( _, WHILE1left, _)) :: rest671)) => let val  result = 
MlyValue.whilecmd (fn _ => let val  (expression as expression1) = 
expression1 ()
 val  (commandseq as commandseq1) = commandseq1 ()
 in (WhileCmd(expression,commandseq))
end)
 in ( LrTable.NT 11, ( result, WHILE1left, OD1right), rest671)
end
|  ( 22, ( ( _, ( _, _, FI1right)) :: ( _, ( MlyValue.commandseq 
commandseq2, _, _)) :: _ :: ( _, ( MlyValue.commandseq commandseq1, _,
 _)) :: _ :: ( _, ( MlyValue.expression expression1, _, _)) :: ( _, (
 _, IF1left, _)) :: rest671)) => let val  result = 
MlyValue.conditionalcmd (fn _ => let val  (expression as expression1)
 = expression1 ()
 val  commandseq1 = commandseq1 ()
 val  commandseq2 = commandseq2 ()
 in (ConditionalCmd(expression,commandseq1,commandseq2))
end)
 in ( LrTable.NT 12, ( result, IF1left, FI1right), rest671)
end
|  ( 23, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( MlyValue.expression 
expression1, _, _)) :: _ :: ( _, ( _, PRINT1left, _)) :: rest671)) =>
 let val  result = MlyValue.printcmd (fn _ => let val  (expression as 
expression1) = expression1 ()
 in (PrintCmd(expression))
end)
 in ( LrTable.NT 10, ( result, PRINT1left, RPAREN1right), rest671)
end
|  ( 24, ( ( _, ( MlyValue.expression expression1, _, expression1right
)) :: _ :: ( _, ( MlyValue.IDEN IDEN1, IDEN1left, _)) :: rest671)) =>
 let val  result = MlyValue.assignmentcmd (fn _ => let val  (IDEN as 
IDEN1) = IDEN1 ()
 val  (expression as expression1) = expression1 ()
 in (AssignmentCmd(IDEN,expression))
end)
 in ( LrTable.NT 9, ( result, IDEN1left, expression1right), rest671)

end
|  ( 25, ( ( _, ( MlyValue.expression expression1, _, expression1right
)) :: ( _, ( _, NEG1left, _)) :: rest671)) => let val  result = 
MlyValue.expression (fn _ => let val  (expression as expression1) = 
expression1 ()
 in (negative(expression))
end)
 in ( LrTable.NT 8, ( result, NEG1left, expression1right), rest671)

end
|  ( 26, ( ( _, ( MlyValue.expression expression1, _, expression1right
)) :: ( _, ( _, INV1left, _)) :: rest671)) => let val  result = 
MlyValue.expression (fn _ => let val  (expression as expression1) = 
expression1 ()
 in (inverse(expression))
end)
 in ( LrTable.NT 8, ( result, INV1left, expression1right), rest671)

end
|  ( 27, ( ( _, ( MlyValue.expression expression2, _, expression2right
)) :: _ :: ( _, ( MlyValue.expression expression1, expression1left, _)
) :: rest671)) => let val  result = MlyValue.expression (fn _ => let
 val  expression1 = expression1 ()
 val  expression2 = expression2 ()
 in (ratadd(expression1,expression2))
end)
 in ( LrTable.NT 8, ( result, expression1left, expression2right), 
rest671)
end
|  ( 28, ( ( _, ( MlyValue.expression expression2, _, expression2right
)) :: _ :: ( _, ( MlyValue.expression expression1, expression1left, _)
) :: rest671)) => let val  result = MlyValue.expression (fn _ => let
 val  expression1 = expression1 ()
 val  expression2 = expression2 ()
 in (ratsub(expression1,expression2))
end)
 in ( LrTable.NT 8, ( result, expression1left, expression2right), 
rest671)
end
|  ( 29, ( ( _, ( MlyValue.expression expression2, _, expression2right
)) :: _ :: ( _, ( MlyValue.expression expression1, expression1left, _)
) :: rest671)) => let val  result = MlyValue.expression (fn _ => let
 val  expression1 = expression1 ()
 val  expression2 = expression2 ()
 in (ratmul(expression1,expression2))
end)
 in ( LrTable.NT 8, ( result, expression1left, expression2right), 
rest671)
end
|  ( 30, ( ( _, ( MlyValue.expression expression2, _, expression2right
)) :: _ :: ( _, ( MlyValue.expression expression1, expression1left, _)
) :: rest671)) => let val  result = MlyValue.expression (fn _ => let
 val  expression1 = expression1 ()
 val  expression2 = expression2 ()
 in (ratdiv(expression1,expression2))
end)
 in ( LrTable.NT 8, ( result, expression1left, expression2right), 
rest671)
end
|  ( 31, ( ( _, ( MlyValue.expression expression2, _, expression2right
)) :: _ :: ( _, ( MlyValue.expression expression1, expression1left, _)
) :: rest671)) => let val  result = MlyValue.expression (fn _ => let
 val  expression1 = expression1 ()
 val  expression2 = expression2 ()
 in (intadd(expression1,expression2))
end)
 in ( LrTable.NT 8, ( result, expression1left, expression2right), 
rest671)
end
|  ( 32, ( ( _, ( MlyValue.expression expression2, _, expression2right
)) :: _ :: ( _, ( MlyValue.expression expression1, expression1left, _)
) :: rest671)) => let val  result = MlyValue.expression (fn _ => let
 val  expression1 = expression1 ()
 val  expression2 = expression2 ()
 in (intsub(expression1,expression2))
end)
 in ( LrTable.NT 8, ( result, expression1left, expression2right), 
rest671)
end
|  ( 33, ( ( _, ( MlyValue.expression expression2, _, expression2right
)) :: _ :: ( _, ( MlyValue.expression expression1, expression1left, _)
) :: rest671)) => let val  result = MlyValue.expression (fn _ => let
 val  expression1 = expression1 ()
 val  expression2 = expression2 ()
 in (intmul(expression1,expression2))
end)
 in ( LrTable.NT 8, ( result, expression1left, expression2right), 
rest671)
end
|  ( 34, ( ( _, ( MlyValue.expression expression2, _, expression2right
)) :: _ :: ( _, ( MlyValue.expression expression1, expression1left, _)
) :: rest671)) => let val  result = MlyValue.expression (fn _ => let
 val  expression1 = expression1 ()
 val  expression2 = expression2 ()
 in (intdiv(expression1,expression2))
end)
 in ( LrTable.NT 8, ( result, expression1left, expression2right), 
rest671)
end
|  ( 35, ( ( _, ( MlyValue.expression expression2, _, expression2right
)) :: _ :: ( _, ( MlyValue.expression expression1, expression1left, _)
) :: rest671)) => let val  result = MlyValue.expression (fn _ => let
 val  expression1 = expression1 ()
 val  expression2 = expression2 ()
 in (intmod(expression1,expression2))
end)
 in ( LrTable.NT 8, ( result, expression1left, expression2right), 
rest671)
end
|  ( 36, ( ( _, ( MlyValue.expression expression2, _, expression2right
)) :: _ :: ( _, ( MlyValue.expression expression1, expression1left, _)
) :: rest671)) => let val  result = MlyValue.expression (fn _ => let
 val  expression1 = expression1 ()
 val  expression2 = expression2 ()
 in (booland(expression1,expression2))
end)
 in ( LrTable.NT 8, ( result, expression1left, expression2right), 
rest671)
end
|  ( 37, ( ( _, ( MlyValue.expression expression2, _, expression2right
)) :: _ :: ( _, ( MlyValue.expression expression1, expression1left, _)
) :: rest671)) => let val  result = MlyValue.expression (fn _ => let
 val  expression1 = expression1 ()
 val  expression2 = expression2 ()
 in (boolor(expression1,expression2))
end)
 in ( LrTable.NT 8, ( result, expression1left, expression2right), 
rest671)
end
|  ( 38, ( ( _, ( MlyValue.expression expression2, _, expression2right
)) :: _ :: ( _, ( MlyValue.expression expression1, expression1left, _)
) :: rest671)) => let val  result = MlyValue.expression (fn _ => let
 val  expression1 = expression1 ()
 val  expression2 = expression2 ()
 in (equal(expression1,expression2))
end)
 in ( LrTable.NT 8, ( result, expression1left, expression2right), 
rest671)
end
|  ( 39, ( ( _, ( MlyValue.expression expression2, _, expression2right
)) :: _ :: ( _, ( MlyValue.expression expression1, expression1left, _)
) :: rest671)) => let val  result = MlyValue.expression (fn _ => let
 val  expression1 = expression1 ()
 val  expression2 = expression2 ()
 in (notequal(expression1,expression2))
end)
 in ( LrTable.NT 8, ( result, expression1left, expression2right), 
rest671)
end
|  ( 40, ( ( _, ( MlyValue.expression expression2, _, expression2right
)) :: _ :: ( _, ( MlyValue.expression expression1, expression1left, _)
) :: rest671)) => let val  result = MlyValue.expression (fn _ => let
 val  expression1 = expression1 ()
 val  expression2 = expression2 ()
 in (less(expression1,expression2))
end)
 in ( LrTable.NT 8, ( result, expression1left, expression2right), 
rest671)
end
|  ( 41, ( ( _, ( MlyValue.expression expression2, _, expression2right
)) :: _ :: ( _, ( MlyValue.expression expression1, expression1left, _)
) :: rest671)) => let val  result = MlyValue.expression (fn _ => let
 val  expression1 = expression1 ()
 val  expression2 = expression2 ()
 in (lesseq(expression1,expression2))
end)
 in ( LrTable.NT 8, ( result, expression1left, expression2right), 
rest671)
end
|  ( 42, ( ( _, ( MlyValue.expression expression2, _, expression2right
)) :: _ :: ( _, ( MlyValue.expression expression1, expression1left, _)
) :: rest671)) => let val  result = MlyValue.expression (fn _ => let
 val  expression1 = expression1 ()
 val  expression2 = expression2 ()
 in (more(expression1,expression2))
end)
 in ( LrTable.NT 8, ( result, expression1left, expression2right), 
rest671)
end
|  ( 43, ( ( _, ( MlyValue.expression expression2, _, expression2right
)) :: _ :: ( _, ( MlyValue.expression expression1, expression1left, _)
) :: rest671)) => let val  result = MlyValue.expression (fn _ => let
 val  expression1 = expression1 ()
 val  expression2 = expression2 ()
 in (moreeq(expression1,expression2))
end)
 in ( LrTable.NT 8, ( result, expression1left, expression2right), 
rest671)
end
|  ( 44, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( MlyValue.expression 
expression1, _, _)) :: ( _, ( _, LPAREN1left, _)) :: rest671)) => let
 val  result = MlyValue.expression (fn _ => let val  (expression as 
expression1) = expression1 ()
 in (expression)
end)
 in ( LrTable.NT 8, ( result, LPAREN1left, RPAREN1right), rest671)
end
|  ( 45, ( ( _, ( MlyValue.RATNUM RATNUM1, RATNUM1left, RATNUM1right))
 :: rest671)) => let val  result = MlyValue.expression (fn _ => let
 val  (RATNUM as RATNUM1) = RATNUM1 ()
 in (rate(RATNUM))
end)
 in ( LrTable.NT 8, ( result, RATNUM1left, RATNUM1right), rest671)
end
|  ( 46, ( ( _, ( MlyValue.BOOLNUM BOOLNUM1, BOOLNUM1left, 
BOOLNUM1right)) :: rest671)) => let val  result = MlyValue.expression
 (fn _ => let val  (BOOLNUM as BOOLNUM1) = BOOLNUM1 ()
 in (boole(BOOLNUM))
end)
 in ( LrTable.NT 8, ( result, BOOLNUM1left, BOOLNUM1right), rest671)

end
|  ( 47, ( ( _, ( MlyValue.INTNUM INTNUM1, INTNUM1left, INTNUM1right))
 :: rest671)) => let val  result = MlyValue.expression (fn _ => let
 val  (INTNUM as INTNUM1) = INTNUM1 ()
 in (inte(INTNUM))
end)
 in ( LrTable.NT 8, ( result, INTNUM1left, INTNUM1right), rest671)
end
| _ => raise (mlyAction i392)
end
val void = MlyValue.VOID
val extract = fn a => (fn MlyValue.program x => x
| _ => let exception ParseInternal
	in raise ParseInternal end) a ()
end
end
structure Tokens : Pi_TOKENS =
struct
type svalue = ParserData.svalue
type ('a,'b) token = ('a,'b) Token.token
fun RATDL (p1,p2) = Token.TOKEN (ParserData.LrTable.T 0,(
ParserData.MlyValue.VOID,p1,p2))
fun INTDL (p1,p2) = Token.TOKEN (ParserData.LrTable.T 1,(
ParserData.MlyValue.VOID,p1,p2))
fun BOOLDL (p1,p2) = Token.TOKEN (ParserData.LrTable.T 2,(
ParserData.MlyValue.VOID,p1,p2))
fun IDEN (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 3,(
ParserData.MlyValue.IDEN (fn () => i),p1,p2))
fun SEMI (p1,p2) = Token.TOKEN (ParserData.LrTable.T 4,(
ParserData.MlyValue.VOID,p1,p2))
fun LBRACE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 5,(
ParserData.MlyValue.VOID,p1,p2))
fun RBRACE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 6,(
ParserData.MlyValue.VOID,p1,p2))
fun LPAREN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 7,(
ParserData.MlyValue.VOID,p1,p2))
fun RPAREN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 8,(
ParserData.MlyValue.VOID,p1,p2))
fun COMMA (p1,p2) = Token.TOKEN (ParserData.LrTable.T 9,(
ParserData.MlyValue.VOID,p1,p2))
fun EOF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 10,(
ParserData.MlyValue.VOID,p1,p2))
fun ASSIGN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 11,(
ParserData.MlyValue.VOID,p1,p2))
fun NEG (p1,p2) = Token.TOKEN (ParserData.LrTable.T 12,(
ParserData.MlyValue.VOID,p1,p2))
fun INV (p1,p2) = Token.TOKEN (ParserData.LrTable.T 13,(
ParserData.MlyValue.VOID,p1,p2))
fun RATPLUS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 14,(
ParserData.MlyValue.VOID,p1,p2))
fun RATSUB (p1,p2) = Token.TOKEN (ParserData.LrTable.T 15,(
ParserData.MlyValue.VOID,p1,p2))
fun RATMUL (p1,p2) = Token.TOKEN (ParserData.LrTable.T 16,(
ParserData.MlyValue.VOID,p1,p2))
fun RATDIV (p1,p2) = Token.TOKEN (ParserData.LrTable.T 17,(
ParserData.MlyValue.VOID,p1,p2))
fun PLUS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 18,(
ParserData.MlyValue.VOID,p1,p2))
fun SUB (p1,p2) = Token.TOKEN (ParserData.LrTable.T 19,(
ParserData.MlyValue.VOID,p1,p2))
fun MUL (p1,p2) = Token.TOKEN (ParserData.LrTable.T 20,(
ParserData.MlyValue.VOID,p1,p2))
fun DIV (p1,p2) = Token.TOKEN (ParserData.LrTable.T 21,(
ParserData.MlyValue.VOID,p1,p2))
fun MOD (p1,p2) = Token.TOKEN (ParserData.LrTable.T 22,(
ParserData.MlyValue.VOID,p1,p2))
fun NOT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 23,(
ParserData.MlyValue.VOID,p1,p2))
fun AND (p1,p2) = Token.TOKEN (ParserData.LrTable.T 24,(
ParserData.MlyValue.VOID,p1,p2))
fun OR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 25,(
ParserData.MlyValue.VOID,p1,p2))
fun EQ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 26,(
ParserData.MlyValue.VOID,p1,p2))
fun NE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 27,(
ParserData.MlyValue.VOID,p1,p2))
fun LT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 28,(
ParserData.MlyValue.VOID,p1,p2))
fun LE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 29,(
ParserData.MlyValue.VOID,p1,p2))
fun GT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 30,(
ParserData.MlyValue.VOID,p1,p2))
fun GE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 31,(
ParserData.MlyValue.VOID,p1,p2))
fun RATNUM (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 32,(
ParserData.MlyValue.RATNUM (fn () => i),p1,p2))
fun BOOLNUM (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 33,(
ParserData.MlyValue.BOOLNUM (fn () => i),p1,p2))
fun INTNUM (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 34,(
ParserData.MlyValue.INTNUM (fn () => i),p1,p2))
fun PRINT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 35,(
ParserData.MlyValue.VOID,p1,p2))
fun IF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 36,(
ParserData.MlyValue.VOID,p1,p2))
fun THEN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 37,(
ParserData.MlyValue.VOID,p1,p2))
fun ELSE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 38,(
ParserData.MlyValue.VOID,p1,p2))
fun FI (p1,p2) = Token.TOKEN (ParserData.LrTable.T 39,(
ParserData.MlyValue.VOID,p1,p2))
fun WHILE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 40,(
ParserData.MlyValue.VOID,p1,p2))
fun OD (p1,p2) = Token.TOKEN (ParserData.LrTable.T 41,(
ParserData.MlyValue.VOID,p1,p2))
fun DO (p1,p2) = Token.TOKEN (ParserData.LrTable.T 42,(
ParserData.MlyValue.VOID,p1,p2))
end
end
