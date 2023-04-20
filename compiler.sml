(* compiler.sml *)
signature PI =
sig
    exception PiError;
    val compile : string -> DataTypes.blockans;
  (*) val walk : DataTypes.blockans -> unit
  val interpret : string -> unit*)
end

structure Pi : PI =
struct
    exception PiError;

    fun compile (fileName) =
    let 
        val inStream = TextIO.openIn fileName;
        val grab : int -> string = fn 
            n => if TextIO.endOfStream inStream
                 then ""
                 else TextIO.inputN (inStream,n);
        val printError : string * int * int -> unit = fn
            (msg,line,col) =>
                print (fileName^"["^Int.toString line^":"
                      ^Int.toString col^"] "^msg^"\n");
        val (tree,rem) = PiParser.parse
                    (15,
                    (PiParser.makeLexer grab fileName),
                    printError,
                    fileName)
            handle PiParser.ParseError => raise PiError;
        (* Close the source program file *)
        val _ = TextIO.closeIn inStream;
    in 
        tree
    end
 (*fun walk(DataTypes.blockans(_  ,[])) = print("Interpretation Done\n")
| walk(DataTypes.blockans(z,x::a)) =
    case x of
       DataTypes.PrintCmd(y) => (print( eval_expr(y) ^ "\n"); walk(DataTypes.blockans(z,a)))
     | _ => print("not implemented yet\n")
    and eval_expr(DataTypes.boole(true)) = "true" 
        | eval_expr(DataTypes.boole(false)) = "false"
        | eval_expr(DataTypes.boolor(a,b)) = let val first = valOf(Bool.fromString(eval_expr(a))) val second = valOf(Bool.fromString(eval_expr(b)))in Bool.toString(first orelse second) end 
        | eval_expr(DataTypes.booland(a,b)) = let val first = valOf(Bool.fromString(eval_expr(a))) val second = valOf(Bool.fromString(eval_expr(b)))in Bool.toString(first andalso second) end 
        | eval_expr(DataTypes.not (e)) = let val first = valOf(Bool.fromString(eval_expr(e)))in Bool.toString(not first) end
        | eval_expr(_) = "not implemented yet"
fun interpret(filename) = walk(compile filename)
*)
   
end;
