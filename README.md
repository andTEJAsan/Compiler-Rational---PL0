Instructions To Run the interpreter
1. Run the command `sudo sml make.sml`
2. In the SML environment type `interpret(inputfile,outputfile)` and press Enter
3. To see the ASTree, type `compile(inputfile)`
3. For read statements, input will be taken from the stdin, press Enter to stop reading
Design Decisions:-
1. To Prevent Shift Reduce Conflicts, the grammar of expressions was modified
2. The modified grammar of expressions
E ::= ~ E
    | inverse E
    | E .+. E
    | E .-. E
    | E .*. E
    | E ./. E
    | E + E
    | E - E
    | E * E
    | E / E
    | E % E
    | ! E
    | E && E
    | E || E
    | E = E
    | E <> E
    | E < E
    | E <= E
    | E > E
    | E >= E
    | intval
    | ratval
    | boolval
    | make_rat(E,E)
    | fromDecimal(E)
    
3. We will do type checking during evaluation of AST
4. For each block the following attributes have been made during AST formation and then changed during execution
# Declarations Table
# Procedure Declarations Table
# Command List
# A Reference list of children blocks (This was built during the ASTree formation, it is a synthesized attribute)
# A Reference to the Parent block
# A Stack of SymbolTables
5. We have made the ASTRee using the sml datatype constructor which allows us to construct recursive data-structures
6. To get the parent references for each block, we started a Depth-First Search from the main block.
7. Whenever an identifier is mentioned , we check its availability in the parent chain of the scope in which it is mentioned.
8. You are not allowed to have vardecls of the same name in the same block (possible to have in different scopes) 
9. You are not allowed to have procdecls of the same name in the same block (possible in different blocks)
10. Whenever a procedure is called, a newsymbol table is added to the symboltable stack to initialize its local variables this will enable recursion to work correctly

11. Precedence orders used 
%left TOR 
%left TAND
%right TNOT
%left TEQ TNE  TLT  TLE  TGT  TGE

%left TSUB TADD TRATSUB TRATADD
%left TMUL TDIV TRATMUL TRATDIV TMOD

 

%right TNEG TINV
12. Nested Comments are not supported like in C language
13. To ignore comments during lexing, States have been used in ML-lex settings
14. Here is a small description of what each file does :-
    core.sml -> contains implementations of BigInt, Rational packages
    pi.lex -> contains the lexer
    pi.yacc -> contains the attribute grammar which build the ASTRee
    datatypes.sml -> contains the datastructures that are used in the ASTRee formation
    glue.sml -> glues the lexer and the parser (Boilerplate)
    pi.cm -> makes sure all the packages are visible
    make.sml -> sets up the interface for the user
    pi.yacc.desc -> A readable description of the generated LR Table used in Parsing
    compiler.sml -> Walks the ASTree and interprets the input file 
    For Interpretation, in the compiler.sml we have used a recursive function to walk the ASTRee

Acknowledgements
________________
1.For the boilerplate I took the help of the example given in the ML-lex Ml-Yacc User guide and documentation
