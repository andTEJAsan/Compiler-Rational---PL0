signature INTERPRETER =
sig
  val walk : DataTypes.blockans -> unit
  val interpret : string -> unit
end;
structure Interpreter:INTERPRETER =
struct
fun walk(_,[]) = print("Interpretation Done\n")
| walk(_,x::a) =
    case x of
       DataTypes.PrintCmd(y) => (print( eval_expr(y) ^ "\n"); walk(a))
     | _ => print("not implemented yet\n")
    and eval_expr(DataTypes.boole(true)) = "true" 
        | eval_expr(DataTypes.boole(false)) = "false"
        | eval_expr(DataTypes.boolor(a,b)) = let val first = valOf(Bool.fromString(eval_expr(a))) val second = valOf(Bool.fromString(eval_expr(b)))in Bool.toString(first orelse second) end 
        | eval_expr(DataTypes.booland(a,b)) = let val first = valOf(Bool.fromString(eval_expr(a))) val second = valOf(Bool.fromString(eval_expr(b)))in Bool.toString(first andalso second) end 
        | eval_expr(DataTypes.not (e)) = let val first = valOf(Bool.fromString(eval_expr(e)))in Bool.toString(not first) end
        | eval_expr(_) = "not implemented yet"
    and interpret(filename) = walk(Pi.compile(filename))
end;