structure DataTypes =
struct
type id = string
datatype prog = Prog;
datatype int_rat_bool_decls = INTa of id | RATa of id | BOOLa of id 
type x = int_rat_bool_decls list
type y = x*x*x
datatype Expression = negative of Expression
                      | inverse of Expression
                      | not of Expression
                      | ratadd of Expression*Expression
                      | ratsub of Expression*Expression
                      | ratmul of Expression*Expression
                      | ratdiv of Expression*Expression
                      | intadd of Expression*Expression
                      | intsub of Expression*Expression
                      | intmul of Expression*Expression
                      | intdiv of Expression*Expression
                      | intmod of Expression*Expression
                      | booland of Expression*Expression
                      | boolor of Expression*Expression
                      | equal of Expression*Expression
                      | notequal of Expression*Expression
                      | less of Expression*Expression
                      | lesseq of Expression*Expression
                      | more of Expression*Expression
                      | moreeq of Expression*Expression
                      | inte of BigInt.bigint
                      | rate of Rational.rational
                      | boole of bool
datatype Cmd = AssignmentCmd of (id*Expression) | PrintCmd of Expression | ConditionalCmd of Expression*(Cmd list)*(Cmd list)
              | WhileCmd of (Expression*(Cmd list))  

datatype blockans = blockans of y*(Cmd list)
 

end;