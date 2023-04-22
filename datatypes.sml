
structure DataTypes =
struct
type id = string
exception InitializationError
datatype prog = Prog;

datatype decls = INT_ of id | RAT_ of id | BOOL_ of id | PROC_ of (id*(blockans ref)) | proc of id
and sym = INTs of BigInt.bigint | RATs of Rational.rational | BOOLs of bool


and  Expression = negative of Expression
                      | inverse of Expression
                      | reference of id
                      | notb of Expression
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
and blockans = blockans of (((decls list)*(decls list)*(decls list))*(((string,decls) HashTable.hash_table) ref))*(Cmd list)*((blockans ref) list)*(blockans ref)*((((string,sym option ) HashTable.hash_table) ref))| Empty

type decls_table = ((string,decls) HashTable.hash_table)
fun extractor(INT_(x)) = x
|   extractor(RAT_(x)) = x
|   extractor(BOOL_(x)) = x
|   extractor(_) = "bogus"

fun initialize_sym([],l) = ()

|   initialize_sym(x::xs,l) =  let val _ = 
(let val f=  HashTable.find (!l) (extractor(x))  in 

(case f of
   SOME a =>( print("Initialization Error , Initialized \" " ^extractor(x)^" \" more then once Can't Initialize a variable more than once\n");raise InitializationError)
 | NONE => ((HashTable.insert (!l) (extractor(x),NONE));())
)
end) in initialize_sym(xs,l) end

end;