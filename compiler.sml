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
end;
open DataTypes
open HashTable
fun eval_expr(E : DataTypes.Expression,env : DataTypes.blockans ref) = 
case E of
   negative(P) => (
    case P of
       INTs(a) => INTs(BigInt.neg(a))
     | RATs(a) => RATs(Rational.neg(a))
     | BOOLs(a) => (print("Type Error, can't apply \"~\" to a boolean expression");raise TypeError)
   )
| inverse(P) => (
    case P of
       INTs(a) => ( print("Type Error, can't apply \"inverse\" to a integer expression");raise TypeError)

     | RATs(a) => RATs(valOf Rational.inverse(a))
     | BOOLs(a) =>( print("Type Error, can't apply \"inverse\" to a boolean expression");raise TypeError)
   )

| refrence(id) => (
   let
     val symt = !(#5 env )
     val obtained = find (symt) (id)
   in

    case obtained of 
    NONE => (print("Variable \""^id^"\" not declared in the given scope but used."); raise NotDeclaredError)
    |SOME NONE => (print("Variable \""^id^"\" not initialized in the given scope but used."); raise NotInitializedError)
    | SOME (INTs(r)) => INTs r
    | SOME (RATs (r)) => RATs r
    | SOME (BOOLs(r)) => BOOLs r
   end
   )
| not(P) => (
    case P of
       INTs(a) => ( print("Type Error, can't apply \"not\" to a integer expression");raise TypeError)

     | RATs(a) =>  ( print("Type Error, can't apply \"not\" to a rational expression");raise TypeError)

     | BOOLs(a) => (BOOLs(not a))

)
|   ratadd(P,Q) =>(

case (P,Q) of 

(RATs p, RATs q) => RATs(Rational.add(p,q))
| _ => (print("Can't use .+. between two expressions of non rational type");raise TypeError)

)
|   ratsub(P,Q) =>(

case (P,Q) of 

(RATs p, RATs q) => RATs(Rational.subtract(p,q))
| _ => (print("Can't use .-. between two expressions of non rational type");raise TypeError)

)
|   ratmul(P,Q) =>(

case (P,Q) of 

(RATs p, RATs q) => RATs(Rational.multiply(p,q))
| _ => (print("Can't use .*. between two expressions of non rational type");raise TypeError)

)
|   ratadd(P,Q) =>(

case (P,Q) of 

(RATs p, RATs q) => RATs(Rational.add(p,q))
| _ => (print("Can't use .+. between two expressions of non rational type");raise TypeError)

)
|   ratadd(P,Q) =>(

case (P,Q) of 

(RATs p, RATs q) => RATs(Rational.add(p,q))
| _ => (print("Can't use .+. between two expressions of non rational type");raise TypeError)

)







close;
close;