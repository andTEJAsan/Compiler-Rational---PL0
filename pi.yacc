open DataTypes
fun get_blockreffromproc(PROC_(_,a : (blockans ref))) = a
|   get_blockreffromproc(_) = ref Empty
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

%nonterm program of blockans | block of blockans
         | declseq of ((decls list)*(decls list)*(decls list))*(decls list)
         | vardecls of (decls list)*(decls list)*(decls list) 
         | procdecls of (decls list)
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

 




%left TSUB TADD
%left TMUL TDIV TMOD

%left TRATSUB TRATADD
%left TRATMUL TRATDIV 

%left TOR 
%left TAND
%left TEQ TNE  TLT  TLE  TGT  TGE
%right TNOT TINV
%right TNEG




%arg (fileName) : string
%start program
%%
program: block (block)
block: declseq commandseq (blockans(declseq,commandseq,(map get_blockreffromproc (#2 declseq)),ref Empty))
declseq: vardecls procdecls (vardecls,procdecls)
        |   (([],[],[]),[])
procdecls: procdef TSEMI procdecls  (procdef::procdecls)
|       ([])
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
|           TNOT expression (not(expression))
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



