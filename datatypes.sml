
structure DataTypes =
struct
open HashTable;
type id = string

datatype prog = Prog;

datatype decls = INT_ of id | RAT_ of id | BOOL_ of id | PROC_ of (id*(blockans ref)) | proc of id


and  Expression = negative of Expression
                      | inverse of Expression
                      | reference of id
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
and Cmd = AssignmentCmd of (id*Expression) | PrintCmd of Expression | ConditionalCmd of Expression*(Cmd list)*(Cmd list)
              | WhileCmd of (Expression*(Cmd list))  
            | CallCmd of id
            | ReadCmd of id
and blockans = blockans of (((decls list)*(decls list)*(decls list))*(((string,decls) HashTable.hash_table) ref))*(Cmd list)*((blockans ref) list)*(blockans ref)| Empty

type decls_table = ((string,decls) HashTable.hash_table)
end;