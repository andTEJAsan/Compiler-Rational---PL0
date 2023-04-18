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
\\001\000\001\000\007\000\002\000\031\000\003\000\031\000\007\000\028\000\000\000\
\\001\000\004\000\010\000\000\000\
\\001\000\004\000\013\000\000\000\
\\001\000\004\000\016\000\000\000\
\\001\000\004\000\019\000\000\000\
\\001\000\005\000\018\000\000\000\
\\001\000\005\000\021\000\000\000\
\\001\000\005\000\023\000\000\000\
\\001\000\007\000\000\000\000\000\
\\025\000\000\000\
\\026\000\000\000\
\\027\000\000\000\
\\029\000\000\000\
\\030\000\000\000\
\\032\000\000\000\
\\033\000\002\000\009\000\000\000\
\\034\000\000\000\
\\035\000\003\000\012\000\000\000\
\\036\000\000\000\
\\037\000\006\000\015\000\000\000\
\"
val actionRowNumbers =
"\000\000\015\000\011\000\010\000\
\\009\000\001\000\017\000\002\000\
\\019\000\012\000\003\000\019\000\
\\005\000\004\000\019\000\006\000\
\\013\000\019\000\007\000\014\000\
\\018\000\016\000\008\000"
val gotoT =
"\
\\001\000\022\000\002\000\004\000\003\000\003\000\004\000\002\000\
\\005\000\001\000\000\000\
\\006\000\006\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\007\000\009\000\000\000\
\\000\000\
\\008\000\012\000\000\000\
\\000\000\
\\000\000\
\\008\000\015\000\000\000\
\\000\000\
\\000\000\
\\008\000\018\000\000\000\
\\000\000\
\\000\000\
\\008\000\020\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\"
val numstates = 23
val numrules = 13
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
 | IDEN of unit ->  (string) | rep of unit ->  (string list)
 | boolvardecls of unit ->  ( ( int* (string list) ) )
 | intvardecls of unit ->  ( ( int* (string list) ) )
 | ratvardecls of unit ->  ( ( int* (string list) ) )
 | vardecls of unit ->  ( ( int* (string list) ) * ( int* (string list) ) * ( int* (string list) ) )
 | declseq of unit ->  ( ( int* (string list) ) * ( int* (string list) ) * ( int* (string list) ) )
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
fn (T 0) => true | (T 2) => true | (T 1) => true | _ => false
val preferred_change : (term list * term list) list = 
nil
val noShift = 
fn (T 6) => true | _ => false
val showTerminal =
fn (T 0) => "RATDL"
  | (T 1) => "INTDL"
  | (T 2) => "BOOLDL"
  | (T 3) => "IDEN"
  | (T 4) => "SEMI"
  | (T 5) => "COMMA"
  | (T 6) => "EOF"
  | _ => "bogus-term"
local open Header in
val errtermvalue=
fn _ => MlyValue.VOID
end
val terms : term list = nil
 $$ (T 6) $$ (T 5) $$ (T 4) $$ (T 2) $$ (T 1) $$ (T 0)end
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
|  ( 1, ( ( _, ( MlyValue.declseq declseq1, declseq1left, 
declseq1right)) :: rest671)) => let val  result = MlyValue.block (fn _
 => let val  (declseq as declseq1) = declseq1 ()
 in (blockans(declseq))
end)
 in ( LrTable.NT 1, ( result, declseq1left, declseq1right), rest671)

end
|  ( 2, ( ( _, ( MlyValue.vardecls vardecls1, vardecls1left, 
vardecls1right)) :: rest671)) => let val  result = MlyValue.declseq
 (fn _ => let val  (vardecls as vardecls1) = vardecls1 ()
 in (vardecls)
end)
 in ( LrTable.NT 2, ( result, vardecls1left, vardecls1right), rest671)

end
|  ( 3, ( rest671)) => let val  result = MlyValue.declseq (fn _ => (
(2,[]),(1,[]),(0,[])))
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
 in ((2,IDEN::rep))
end)
 in ( LrTable.NT 4, ( result, RATDL1left, SEMI1right), rest671)
end
|  ( 6, ( rest671)) => let val  result = MlyValue.ratvardecls (fn _ =>
 ((2,[])))
 in ( LrTable.NT 4, ( result, defaultPos, defaultPos), rest671)
end
|  ( 7, ( ( _, ( _, _, SEMI1right)) :: ( _, ( MlyValue.rep rep1, _, _)
) :: ( _, ( MlyValue.IDEN IDEN1, _, _)) :: ( _, ( _, INTDL1left, _))
 :: rest671)) => let val  result = MlyValue.intvardecls (fn _ => let
 val  (IDEN as IDEN1) = IDEN1 ()
 val  (rep as rep1) = rep1 ()
 in ((1,IDEN::rep))
end)
 in ( LrTable.NT 5, ( result, INTDL1left, SEMI1right), rest671)
end
|  ( 8, ( rest671)) => let val  result = MlyValue.intvardecls (fn _ =>
 ((1,[])))
 in ( LrTable.NT 5, ( result, defaultPos, defaultPos), rest671)
end
|  ( 9, ( ( _, ( _, _, SEMI1right)) :: ( _, ( MlyValue.rep rep1, _, _)
) :: ( _, ( MlyValue.IDEN IDEN1, _, _)) :: ( _, ( _, BOOLDL1left, _))
 :: rest671)) => let val  result = MlyValue.boolvardecls (fn _ => let
 val  (IDEN as IDEN1) = IDEN1 ()
 val  (rep as rep1) = rep1 ()
 in ((0,IDEN::rep))
end)
 in ( LrTable.NT 6, ( result, BOOLDL1left, SEMI1right), rest671)
end
|  ( 10, ( rest671)) => let val  result = MlyValue.boolvardecls (fn _
 => ((0,[])))
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
fun COMMA (p1,p2) = Token.TOKEN (ParserData.LrTable.T 5,(
ParserData.MlyValue.VOID,p1,p2))
fun EOF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 6,(
ParserData.MlyValue.VOID,p1,p2))
end
end
