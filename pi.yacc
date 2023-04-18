open DataTypes
%%
%name Pi
%term RATDL | INTDL | BOOLDL
         | IDEN of string
         | SEMI
         | LBRACE
         | RBRACE
         | LPAREN
         | RPAREN
         | COMMA
         | EOF
         | ASSIGN
         | NEG
         | INV
         | RATPLUS
         | RATSUB
         | RATMUL
         | RATDIV
         | PLUS
         | SUB
         | MUL
         | DIV
         | MOD
         | NOT
         | AND
         | OR
         | EQ
         | NE
         | LT
         | LE
         | GT
         | GE
         | RATNUM of Rational.rational
         | BOOLNUM of bool
         | INTNUM of BigInt.bigint
         | PRINT
         | IF 
         | THEN
         | ELSE
         | FI
         | WHILE
         | OD
         | DO
%nonterm program of blockans | block of blockans
         | declseq of (int_rat_bool_decls list)*(int_rat_bool_decls list)*(int_rat_bool_decls list) 
         | vardecls of (int_rat_bool_decls list)*(int_rat_bool_decls list)*(int_rat_bool_decls list) 
         | ratvardecls of (int_rat_bool_decls list)
         | intvardecls of  (int_rat_bool_decls list)
         | boolvardecls of (int_rat_bool_decls list)
         | rep of string list
         | expression of Expression
         | assignmentcmd of Cmd
         | printcmd of Cmd
         | whilecmd of Cmd
         | conditionalcmd of Cmd
         | command of Cmd
         | res of Cmd list
         | commandseq of Cmd list



%pos int
%eop EOF
%noshift EOF
%nonassoc RATDL EOF BOOLDL INTDL SEMI COMMA
%verbose
%keyword RATDL BOOLDL INTDL WHILE DO OD IF FI THEN ELSE PRINT

 




%left SUB PLUS
%left MUL DIV MOD

%left RATSUB RATPLUS
%left RATMUL RATDIV 

%left OR AND 
%left EQ NE  LT  LE  GT  GE
%right NOT INV
%right NEG




%arg (fileName) : string
%start program
%%
program: block (block)
block: declseq commandseq (blockans(declseq,commandseq))
declseq: vardecls (vardecls)
        |   (([],[],[]))
vardecls: ratvardecls intvardecls boolvardecls ((ratvardecls,intvardecls,boolvardecls))
ratvardecls: RATDL IDEN rep SEMI (map RATa (IDEN::rep))
            | ([])
intvardecls: INTDL IDEN rep SEMI (map INTa (IDEN::rep))
            | ([])
boolvardecls: BOOLDL IDEN rep SEMI (map BOOLa (IDEN::rep))
                | ([])
rep: COMMA IDEN rep (IDEN::rep)
    | ([])
commandseq:     LBRACE RBRACE ([])
        |       LBRACE command SEMI res RBRACE (command::res)
res: command SEMI res (command::res)
        | ([])
command: assignmentcmd (assignmentcmd)
|       printcmd (printcmd)
|       conditionalcmd (conditionalcmd)
|       whilecmd (whilecmd)
whilecmd: WHILE expression DO commandseq OD (WhileCmd(expression,commandseq))
conditionalcmd: IF expression THEN commandseq ELSE commandseq FI (ConditionalCmd(expression,commandseq1,commandseq2)) 
printcmd: PRINT LPAREN expression RPAREN (PrintCmd(expression))
assignmentcmd: IDEN ASSIGN expression (AssignmentCmd(IDEN,expression))
expression: NEG expression (negative(expression))
|           INV expression (inverse(expression))
|           expression RATPLUS expression (ratadd(expression1,expression2))
|           expression RATSUB expression (ratsub(expression1,expression2))
|           expression RATMUL expression (ratmul(expression1,expression2))
|           expression RATDIV expression (ratdiv(expression1,expression2))
|           expression PLUS expression (intadd(expression1,expression2))
|           expression SUB expression (intsub(expression1,expression2))
|           expression MUL expression (intmul(expression1,expression2))
|           expression DIV expression (intdiv(expression1,expression2))
|           expression MOD expression (intmod(expression1,expression2))
|           expression AND expression (booland(expression1,expression2))
|           expression OR expression (boolor(expression1,expression2))
|           expression EQ expression (equal(expression1,expression2))
|           expression NE expression (notequal(expression1,expression2))
|           expression LT expression (less(expression1,expression2))
|           expression LE expression (lesseq(expression1,expression2))
|           expression GT expression (more(expression1,expression2))
|           expression GE expression (moreeq(expression1,expression2))
|           LPAREN expression RPAREN (expression)
|           RATNUM (rate(RATNUM))
|           BOOLNUM (boole(BOOLNUM))
|           INTNUM (inte(INTNUM))



