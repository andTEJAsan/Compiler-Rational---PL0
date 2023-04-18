open DataTypes
%%
%name Pi
%term RATDL | INTDL | BOOLDL
         | IDEN of string
         | SEMI
         | COMMA
         | EOF
%nonterm program of blockans | block of blockans
         | declseq of (int* (string list))*(int* (string list))*(int* (string list)) 
         | vardecls of (int* (string list))*(int* (string list))*(int* (string list)) 
         | ratvardecls of (int* (string list))
         | intvardecls of  (int* (string list))
         | boolvardecls of (int* (string list))
         | rep of string list


%pos int
%eop EOF
%noshift EOF
%nonassoc RATDL EOF BOOLDL INTDL SEMI COMMA
%verbose
%keyword RATDL BOOLDL INTDL
%arg (fileName) : string
%start program
%%
program: block (block)
block: declseq (blockans(declseq))
declseq: vardecls (vardecls)
        |   ((2,[]),(1,[]),(0,[]))
vardecls: ratvardecls intvardecls boolvardecls ((ratvardecls,intvardecls,boolvardecls))
ratvardecls: RATDL IDEN rep SEMI ((2,IDEN::rep))
            | ((2,[]))
intvardecls: INTDL IDEN rep SEMI ((1,IDEN::rep))
            | ((1,[]))
boolvardecls: BOOLDL IDEN rep SEMI ((0,IDEN::rep))
                | ((0,[]))
rep: COMMA IDEN rep (IDEN::rep)
    | ([])