open DataTypes
fun get_blockreffromproc(PROC_(_,a : (blockans ref))) = a
|   get_blockreffromproc(_) = ref Empty
fun getlast(DataTypes.blockans(a,b,c,d,e))= d
|   getlast(DataTypes.Empty) = ref DataTypes.Empty
fun grand (n1:DataTypes.blockans )(x) = getlast(!x):= n1;
 fun dfs(bl as DataTypes.blockans(a,b,c,d,e) : DataTypes.blockans):unit =  (((map (grand(bl))) c) ;  let
  fun repeater([]) = ()
  |   repeater(x::tl) = ((dfs(!x);repeater(tl));())
in
        repeater(c)
end) 
fun get_empty() = ref(let val ht : (string, decls) HashTable.hash_table = HashTable.mkTable(HashString.hashString, op=)(49, Domain)
in ht end)
fun get_emptysym() = ref(let val ht : (string, sym option) HashTable.hash_table = HashTable.mkTable(HashString.hashString, op=)(49, Domain)
in ht end)
fun get_id_from_proc(PROC_(x,y)) = x
|   get_id_from_proc(_)  = "bogus"
fun getter(l) = let val shimt = get_emptysym() in initialize_sym(l,shimt);shimt end 

fun getterl(l) = ref([getter(l)])
fun newsym(env : blockans ref) = case (!env) of
   DataTypes.blockans(((z1,z2,z3),y),b,c,d,e) => getter((z1@z2@z3))
 | _ => (print("It should never come to this\n");raise DataTypes.InitializationError)
fun check(id,symt) = (
        case (HashTable.find(symt)(id) ) of
           SOME a => (print("Can't Have more than One procedure declaration with the name \""^id^"\" in the same block"); raise DeclarationError)
         | _ => ()
)

%%
%name Pi
%term TRATIONAL | TINTEGER | TBOOLEAN
         | TIDEN of string
         | TSEMI
         | TLBRACE
         | TRBRACE
         | TLPAREN
         | TRPAREN
         | TCOMMA
         | TEOF
         | TASSIGN
         | TNEG
         | TINV
         | TRATADD
         | TRATSUB
         | TRATMUL
         | TRATDIV
         | TADD
         | TSUB
         | TMUL
         | TDIV
         | TMOD
         | TNOT
         | TAND
         | TOR
         | TEQ
         | TNE
         | TLT
         | TLE
         | TGT
         | TGE
         | TRATNUM of Rational.rational
         | TBOOLNUM of bool
         | TINTNUM of BigInt.bigint
         | TPRINT
         | TIF 
         | TTHEN
         | TELSE
         | TFI
         | TWHILE
         | TOD
         | TDO
         | TPROCEDURE
         | TREAD
         | TCALL
         | TMAKERAT
         | TFROMDECIMAL
%nonterm program of blockans | block of blockans
         | declseq of ((decls list)*(decls list)*(decls list))*( decls_table ref)
         | vardecls of (decls list)*(decls list)*(decls list) 
         | procdecls of ( decls_table ref)
         | procdef of (decls)
         | ratvardecls of (decls list)
         | intvardecls of  (decls list)
         | boolvardecls of (decls list)
         | rep of string list
         | expression of Expression
         | assignmentcmd of Cmd
         | printcmd of Cmd
         | readcmd of Cmd
         | callcmd of Cmd
         | whilecmd of Cmd
         | conditionalcmd of Cmd
         | command of Cmd
         | res of Cmd list
         | commandseq of Cmd list



%pos int
%eop TEOF
%noshift TEOF
%nonassoc TRATIONAL TEOF TBOOLEAN TINTEGER TSEMI TCOMMA
%verbose
%keyword TRATIONAL TBOOLEAN TINTEGER TWHILE TDO TOD TIF TFI TTHEN TELSE TPRINT TPROCEDURE TREAD TCALL

 




%left TOR 
%left TAND
%right TNOT
%left TEQ TNE  TLT  TLE  TGT  TGE

%left TSUB TADD TRATSUB TRATADD
%left TMUL TDIV TRATMUL TRATDIV TMOD

 

%right TNEG TINV




%arg (fileName) : string
%start program
%%
program: block (dfs(block);block)
block: declseq commandseq (blockans(declseq,commandseq,(map get_blockreffromproc (HashTable.listItems(!(#2 declseq)))),ref Empty,getterl((#1(#1 declseq))@(#2(#1 declseq))@(#3(#1 declseq)))))
declseq: vardecls procdecls (vardecls,procdecls)
        |   (([],[],[]),get_empty())
procdecls: procdef TSEMI procdecls  (check(get_id_from_proc(procdef),(!procdecls));HashTable.insert(!procdecls)(get_id_from_proc(procdef),procdef);procdecls)
|       (get_empty())
procdef: TPROCEDURE TIDEN block (PROC_(TIDEN,ref block))
vardecls: ratvardecls intvardecls boolvardecls ((ratvardecls,intvardecls,boolvardecls))
ratvardecls: TRATIONAL TIDEN rep TSEMI (map RAT_ (TIDEN::rep))
            | ([])
intvardecls: TINTEGER TIDEN rep TSEMI (map INT_ (TIDEN::rep))
            | ([])
boolvardecls: TBOOLEAN TIDEN rep TSEMI (map BOOL_ (TIDEN::rep))
                | ([])
rep: TCOMMA TIDEN rep (TIDEN::rep)
    | ([])
commandseq:     TLBRACE TRBRACE ([])
        |       TLBRACE command TSEMI res TRBRACE (command::res)
res: command TSEMI res (command::res)
        | ([])
command: assignmentcmd (assignmentcmd)
|       printcmd (printcmd)
|       conditionalcmd (conditionalcmd)
|       whilecmd (whilecmd)
|       callcmd (callcmd)
|       readcmd (readcmd)
readcmd: TREAD TLPAREN TIDEN TRPAREN (ReadCmd(TIDEN))
callcmd: TCALL TIDEN (CallCmd(TIDEN))
whilecmd: TWHILE expression TDO commandseq TOD (WhileCmd(expression,commandseq))
conditionalcmd: TIF expression TTHEN commandseq TELSE commandseq TFI (ConditionalCmd(expression,commandseq1,commandseq2)) 
printcmd: TPRINT TLPAREN expression TRPAREN (PrintCmd(expression))
assignmentcmd: TIDEN TASSIGN expression (AssignmentCmd(TIDEN,expression))
expression: TNEG expression (negative(expression))
|           TINV expression (inverse(expression))
|           TNOT expression (notb(expression))
|           TIDEN (reference(TIDEN))
|           expression TRATADD expression (ratadd(expression1,expression2))
|           expression TRATSUB expression (ratsub(expression1,expression2))
|           expression TRATMUL expression (ratmul(expression1,expression2))
|           expression TRATDIV expression (ratdiv(expression1,expression2))
|           expression TADD expression (intadd(expression1,expression2))
|           expression TSUB expression (intsub(expression1,expression2))
|           expression TMUL expression (intmul(expression1,expression2))
|           expression TDIV expression (intdiv(expression1,expression2))
|           expression TMOD expression (intmod(expression1,expression2))
|           expression TAND expression (booland(expression1,expression2))
|           expression TOR expression (boolor(expression1,expression2))
|           expression TEQ expression (equal(expression1,expression2))
|           expression TNE expression (notequal(expression1,expression2))
|           expression TLT expression (less(expression1,expression2))
|           expression TLE expression (lesseq(expression1,expression2))
|           expression TGT expression (more(expression1,expression2))
|           expression TGE expression (moreeq(expression1,expression2))
|           TLPAREN expression TRPAREN (expression)
|           TRATNUM (rate(TRATNUM))
|           TBOOLNUM (boole(TBOOLNUM))
|           TINTNUM (inte(TINTNUM))
|           TMAKERAT TLPAREN expression TCOMMA expression TRPAREN (makerat(expression1,expression2))
|           TFROMDECIMAL TLPAREN TRATNUM TRPAREN (rate(TRATNUM))

