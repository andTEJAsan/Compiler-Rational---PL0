(* compiler.sml *)

structure Interpreter =
struct
exception TypeError
exception NotDeclaredError
exception NotInitializedError  
(*fun parent(env : DataTypes.blockans ref) =
case (!env) of
   DataTypes.blockans(a,b,c,d,e) => 
 | pat2 => body2*)
fun eval_expr(E : DataTypes.Expression,env : DataTypes.blockans ref) = 
case E of
   DataTypes.negative(P) => (
    case eval_expr(P,env) of
       DataTypes.INTs(a) => DataTypes.INTs(BigInt.neg(a))
     | DataTypes.RATs(a) => DataTypes.RATs(Rational.neg(a))
     | DataTypes.BOOLs(a) => (print("Type Error, can't apply \"~\" to a boolean expression");raise TypeError)
   )
| DataTypes.inverse(P) => (
    case eval_expr(P,env) of
       DataTypes.INTs(a) => ( print("Type Error, can't apply \"inverse\" to a integer expression");raise TypeError)

     | DataTypes.RATs(a) => DataTypes.RATs(valOf (Rational.inverse(a)))
     | DataTypes.BOOLs(a) =>( print("Type Error, can't apply \"inverse\" to a boolean expression");raise TypeError)
)

| DataTypes.reference(id) => (
   let
     val symt = !( (let val bt = (!env) in
      case bt of DataTypes.blockans(a,b,c,d,e) => e 
    | _ =>  ref(let val ht : (string, DataTypes.sym option) HashTable.hash_table = HashTable.mkTable(HashString.hashString, op=)(17, Domain)
in ht end)
      end))
     val obtained = HashTable.find (symt) (id)
   in

    case obtained of 
    NONE => (print("Variable \""^id^"\" not declared in the given scope but used."); raise NotDeclaredError)
    | SOME(SOME (DataTypes.INTs(r))) => DataTypes.INTs r
    | SOME( SOME(DataTypes.RATs (r)) )=> DataTypes.RATs r
    | SOME( SOME(DataTypes.BOOLs(r)) )=> DataTypes.BOOLs r

    | SOME NONE => (print("Variable \""^id^"\" not initialized in the given scope but used."); raise NotInitializedError)
   end
)
| DataTypes.notb(P) => (
    case eval_expr(P,env) of
       DataTypes.INTs(a) => ( print("Type Error, can't apply \"not\" to a integer expression");raise TypeError)

     | DataTypes.RATs(a) =>  ( print("Type Error, can't apply \"not\" to a rational expression");raise TypeError)

     | DataTypes.BOOLs(a) => (DataTypes.BOOLs(not a))

)
|   DataTypes.ratadd(P,Q) =>(

case (eval_expr(P,env),eval_expr(Q,env)) of 

(DataTypes.RATs p, DataTypes.RATs q) => DataTypes.RATs(Rational.add(p,q))
| _ => (print("Can't use .+. between two expressions of non rational type");raise TypeError)

)
|   DataTypes.ratsub(P,Q) =>(

case  (eval_expr(P,env),eval_expr(Q,env)) of 

(DataTypes.RATs p, DataTypes.RATs q) => DataTypes.RATs(Rational.subtract(p,q))
| _ => (print("Can't use .-. between two expressions of non rational type");raise TypeError)

)
|   DataTypes.ratmul(P,Q) =>(

case  (eval_expr(P,env),eval_expr(Q,env)) of 

(DataTypes.RATs p, DataTypes.RATs q) => DataTypes.RATs(Rational.multiply(p,q))
| _ => (print("Can't use .*. between two expressions of non rational type");raise TypeError)

)
|   DataTypes.ratdiv(P,Q) =>(

case (eval_expr(P,env),eval_expr(Q,env))  of 

(DataTypes.RATs p, DataTypes.RATs q) => DataTypes.RATs(valOf(Rational.divide(p,q)))
| _ => (print("Can't use ./. between two expressions of non rational type");raise TypeError)

)
|   DataTypes.intadd(P,Q) =>(

case (eval_expr(P,env),eval_expr(Q,env)) of 

(DataTypes.INTs p, DataTypes.INTs q) => DataTypes.INTs(BigInt.add(p,q))
| _ => (print("Can't use + between two expressions of non integral type");raise TypeError)

)
|   DataTypes.intsub(P,Q) =>(

case (eval_expr(P,env),eval_expr(Q,env)) of 

(DataTypes.INTs p, DataTypes.INTs q) => DataTypes.INTs(BigInt.sub(p,q))
| _ => (print("Can't use - between two expressions of non integral type\n");raise TypeError)

)
|   DataTypes.intmul(P,Q) =>(

case (eval_expr(P,env),eval_expr(Q,env)) of 

(DataTypes.INTs p, DataTypes.INTs q) => DataTypes.INTs(BigInt.mul(p,q))
| _ => (print("Can't use * between two expressions of non integral type\n");raise TypeError)

)
|   DataTypes.intdiv(P,Q) =>(

case (eval_expr(P,env),eval_expr(Q,env)) of 

(DataTypes.INTs p, DataTypes.INTs q) => DataTypes.INTs(valOf(BigInt.div(p,q)))
| _ => (print("Can't use / between two expressions of non integral type\n");raise TypeError)

)
|   DataTypes.intmod(P,Q) =>(

case (eval_expr(P,env),eval_expr(Q,env)) of 

(DataTypes.INTs p, DataTypes.INTs q) => DataTypes.INTs(valOf(BigInt.mod(p,q)))
| _ => (print("Can't use % between two expressions of non integral type\n");raise TypeError)

)

|   DataTypes.booland(P,Q) =>(

case (eval_expr(P,env),eval_expr(Q,env)) of 

(DataTypes.BOOLs p, DataTypes.BOOLs q) => DataTypes.BOOLs(p andalso q)
| _ => (print("Can't use && between two expressions of non boolean type\n");raise TypeError)

)
|   DataTypes.boolor(P,Q) =>(

case (eval_expr(P,env),eval_expr(Q,env)) of 

(DataTypes.BOOLs p, DataTypes.BOOLs q) => DataTypes.BOOLs(p orelse q)
| _ => (print("Can't use || between two expressions of non boolean type\n");raise TypeError)

)


|   DataTypes.equal(P,Q) => (

  case (eval_expr(P,env),eval_expr(Q,env)) of 
  (DataTypes.BOOLs p, DataTypes.BOOLs q) => (DataTypes.BOOLs(p = q))
| (DataTypes.INTs p, DataTypes.INTs q) => (DataTypes.BOOLs(BigInt.equals(p,q)))
| (DataTypes.RATs p, DataTypes.RATs q) => (DataTypes.BOOLs(Rational.equal(p,q)))
| _ => (print("Can't check for equality for non-equal types\n");raise TypeError)

)
|   DataTypes.notequal(P,Q) => (

  case (eval_expr(P,env),eval_expr(Q,env)) of 
  (DataTypes.BOOLs p, DataTypes.BOOLs q) => (DataTypes.BOOLs(not(p = q)))
| (DataTypes.INTs p, DataTypes.INTs q) => (DataTypes.BOOLs(not(BigInt.equals(p,q))))
| (DataTypes.RATs p, DataTypes.RATs q) => (DataTypes.BOOLs(not(Rational.equal(p,q))))
| _ => (print("Can't check for nonequality for non-equal types\n");raise TypeError)

)

|   DataTypes.less(P,Q) => (

  case (eval_expr(P,env),eval_expr(Q,env)) of 
(DataTypes.INTs p, DataTypes.INTs q) => DataTypes.BOOLs(BigInt.>(q,p))
| (DataTypes.RATs p, DataTypes.RATs q) => DataTypes.BOOLs(Rational.less(p,q))
| (DataTypes.BOOLs p, DataTypes.BOOLs q) => (print("Can't check for < for BOOLEAN Types\n");raise TypeError)
| _ => (print("Can't compare  between non equal types");raise TypeError)
)
|   DataTypes.lesseq(P,Q) => (

  case (eval_expr(P,env),eval_expr(Q,env)) of 
(DataTypes.INTs p, DataTypes.INTs q) => DataTypes.BOOLs((BigInt.>(q,p)) orelse BigInt.equals(p,q))
| (DataTypes.RATs p, DataTypes.RATs q) => DataTypes.BOOLs((Rational.less(p,q)) orelse (Rational.equal(p,q)))
| (DataTypes.BOOLs p, DataTypes.BOOLs q) => (print("Can't check for <= for BOOLEAN Types\n");raise TypeError)
| _ => (print("Can't check for non-equality/equality between non equal types");raise TypeError)
)

|   DataTypes.more(P,Q) => (

  case (eval_expr(P,env),eval_expr(Q,env)) of 
(DataTypes.INTs p, DataTypes.INTs q) => DataTypes.BOOLs(BigInt.>(p,q))
| (DataTypes.RATs p, DataTypes.RATs q) => DataTypes.BOOLs(Rational.less(q,p))
| (DataTypes.BOOLs p, DataTypes.BOOLs q) => (print("Can't check for > for BOOLEAN Types\n");raise TypeError)
| _ => (print("Can't check for non-equality/equality between non equal types");raise TypeError)
)
|   DataTypes.moreeq(P,Q) => (

  case (eval_expr(P,env),eval_expr(Q,env)) of 
(DataTypes.INTs p, DataTypes.INTs q) => DataTypes.BOOLs((BigInt.>(p,q)) orelse BigInt.equals(p,q))
| (DataTypes.RATs p, DataTypes.RATs q) => DataTypes.BOOLs((Rational.less(q,p)) orelse (Rational.equal(p,q)))
| (DataTypes.BOOLs p, DataTypes.BOOLs q) => (print("Can't check for => for BOOLEAN Types\n");raise TypeError)
| _ => (print("Can't check for non-equality/equality between non equal types");raise TypeError)
)
|   DataTypes.inte(X) => (DataTypes.INTs X)
|   DataTypes.rate(X) => (DataTypes.RATs X)
|   DataTypes.boole(X) =>(DataTypes.BOOLs X)


end;

signature PI =


sig
    exception PiError;
    val compile : string -> DataTypes.blockans;
    val dummy : string -> DataTypes.sym;
  (* val walk : DataTypes.blockans -> unit
  val interpret : string -> unit*)
end;

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
fun dummy(filename) = 
    let
      val compiled = compile filename
      val env = ref compiled
      val cmds = (case compiled of
          DataTypes.blockans(a,b,c,d,e) => b
          | DataTypes.Empty => []
      )
      val command = (case (hd cmds) of
         DataTypes.PrintCmd(E) => E
       | _ => raise PiError
      )
    in
      Interpreter.eval_expr(command,env) 
    end
end;