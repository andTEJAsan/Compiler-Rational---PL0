(* compiler.sml *)

structure Interpreter =
struct
exception FoolError
exception TypeError
exception NotDeclaredError
exception NotInitializedError  
exception InputError
val outs = ref (TextIO.stdOut)
fun print(s:string)=
(


    (TextIO.output(!outs,s); TextIO.flushOut(!outs))
)
fun xtractor(s : DataTypes.id) (x : DataTypes.decls) = 

case x of 
DataTypes.INT_(h) => (if(h=s) then SOME (DataTypes.INT_(s)) else NONE)
| DataTypes.BOOL_(h) => (if(h=s) then SOME (DataTypes.BOOL_(s)) else NONE)
| DataTypes.RAT_(h) => (if (h=s) then SOME(DataTypes.RAT_(s)) else NONE)
| _ => NONE
fun getdectype(s)([]) = NONE
|   getdectype(s)(x::xs) = case  xtractor(s)(x) of
   NONE => getdectype(s)(xs)
 | SOME(a) => SOME(a)
fun get_decls(env : DataTypes.blockans ref)=
case (!env) of 
  DataTypes.blockans(((l,m,n),k),b,c,d,e) => (l@m@n)
| _ => (print("WTF NO\n");raise FoolError)
fun get_cmdseq(env : DataTypes.blockans ref) = 

case (!env) of
   DataTypes.blockans(a,b,c,d,e) => (b)
 | _  => (print("WTF NO\n");raise FoolError)
fun get_proc_symt(env : DataTypes.blockans ref) = 
case (!env) of
   DataTypes.blockans(a,b,c,d,e) => (
    (#2 a)
   )
 | _ => (print("WTF NO\n");raise FoolError)
fun get_var_symt(env : DataTypes.blockans ref) = 
case (!env) of
   DataTypes.blockans(a,b,c,d,e) => (hd(!e))
 | _ => (print("WTF NO\n");raise FoolError)

fun parent(env : DataTypes.blockans ref) =

case (!env) of
   DataTypes.blockans(a,b,c,d,e) => d
 | DataTypes.Empty => (print("I dont have a parent :( \n");ref DataTypes.Empty)
fun search_id(i : DataTypes.id,env: DataTypes.blockans ref) = 
let
  val symt =  !(get_var_symt(env))
  val search_result = ((HashTable.find symt) i)
in
  case search_result of
     NONE => (
      case parent(env) of 
      ref(DataTypes.Empty) =>(print("Variable \""^i^"\" hasn't been declared in any appropriate scope\n");raise NotDeclaredError)
     | _ => search_id(i,parent(env))
     )

   |  _ => (env)
end
fun search_procid(i: DataTypes.id, env: DataTypes.blockans ref) = 
let
  val symt = !(get_proc_symt(env))
  val search_result = ((HashTable.find symt) i)
in
case search_result of 
     NONE => (
      case parent(env) of 
      ref(DataTypes.Empty) =>(print("Procedure \""^i^"\" hasn't been declared in any appropriate scope\n");raise NotDeclaredError)
     | _ => search_procid(i,parent(env))
     )
    | _ => (env)

end
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
    val symt = !(get_var_symt(env))
     val obtained = HashTable.find (symt) (id)
   in

    case obtained of 
    NONE => (
      case parent(env) of
       ref (DataTypes.Empty) => (print("Variable \""^id^"\" not declared in any scope but used."); raise NotDeclaredError)
     | _  => eval_expr(E,parent(env))
    )
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
|   DataTypes.makerat(P,Q) => (
  case (eval_expr(P,env),eval_expr(Q,env)) of
(DataTypes.INTs p, DataTypes.INTs q) => DataTypes.RATs(valOf(Rational.make_rat(p,q)))
| _ => (print("TypeError, Both arguments to make_rat should be of type int * int");raise TypeError)
)
|   DataTypes.rata(P) =>(
  case eval_expr(P,env) of
     DataTypes.INTs p => DataTypes.RATs(valOf(Rational.rat(p)))
   | _ => raise FoolError
)
|   DataTypes.inte(X) => (DataTypes.INTs X)
|   DataTypes.rate(X) => (DataTypes.RATs X)
|   DataTypes.boole(X) =>(DataTypes.BOOLs X)

fun execute_single(cmd : DataTypes.Cmd, env : DataTypes.blockans ref) = 
case cmd of
   DataTypes.PrintCmd(E) => (
    let
      val ans = eval_expr(E,env)
    in
    case ans of 
    DataTypes.BOOLs(true) => (print("tt\n");())
    | DataTypes.BOOLs(false) => (print("ff\n");())
    | DataTypes.INTs(i) => (print(BigInt.toString(i)^"\n");())
    | DataTypes.RATs(r) => (print(Rational.showDecimal(r)^"\n");())
    end
   )
 | DataTypes.AssignmentCmd(s,E) => (
  let
    val present_scope = search_id(s,env) (*type blockans ref*)
    val symtab = (!(get_var_symt(present_scope)))
    val newval = SOME (eval_expr(E,env)) (* type sym*)
    val toinsert = (s,newval)
  in
    HashTable.insert symtab toinsert
  end

 )
 | DataTypes.ConditionalCmd(cond,A,B) => (

let
  val verdict = eval_expr(cond,env)
in
  case verdict of
     DataTypes.BOOLs(true) => execute_multi(A,env)
   | DataTypes.BOOLs(false) => execute_multi(B,env)
   | _ => (print("Only boolean expressions allowed as the decider, You have entered the wrong type of expression instead. Please Check\n");raise TypeError)
end

 )
 | DataTypes.WhileCmd(cond, cmdl) =>(
let
  val verdict = eval_expr(cond,env)
in
  case verdict of
    DataTypes.BOOLs(true) => (execute_multi(cmdl, env);execute_single(cmd,env))
   | DataTypes.BOOLs(false) => ()
   | _ => (print("While Expression should be a boolean expression\n");raise TypeError)
end
 )
 | DataTypes.CallCmd(id) => (
 let
   val present_scope = search_procid(id,env)
   val proc_symt = !(get_proc_symt(present_scope))
   val scope_toexecute = (case (HashTable.lookup) proc_symt id  of 
   DataTypes.PROC_(yu,tyt) => tyt
   | _ => (print("It should never come to this\n");raise FoolError)
   )
   val cmdseq = get_cmdseq(scope_toexecute)
 in
   (DataTypes.pusher(scope_toexecute);execute_multi(cmdseq, scope_toexecute);DataTypes.popper(scope_toexecute))
 end
 )
 | DataTypes.ReadCmd(id) => (
let
  val present_scope = search_id(id,env)
  val declist = get_decls(present_scope)
  
  val gettype = getdectype(id)(declist)
in
case gettype of
   NONE => (
    print("Cannot Read to \""^id^"\" as it has not been declared");
    raise NotDeclaredError
   )
 | SOME(DataTypes.INT_(a)) => (
let
  val inputline =(TextIO.print("Enter Integer Number for \""^a^"\": \n");( case (TextIO.inputLine TextIO.stdIn) of 
  NONE => raise InputError
  | SOME s => s
  ))

  val num = String.substring(inputline,0,size(inputline)-1)
  val symentryexp = DataTypes.inte(BigInt.fromString(num))
  val command = DataTypes.AssignmentCmd(id,symentryexp)
in
  (execute_single(command,env))
end
 )
| SOME(DataTypes.BOOL_(a)) => (
let
  val inputline =(TextIO.print("Enter Boolean Value for \""^a^"\" :\n");( case (TextIO.inputLine TextIO.stdIn) of 
  NONE => raise InputError
  | SOME s => s
  ))

  val num = String.substring(inputline,0,size(inputline)-1)
  val symentryexp = (case num of
  "tt" => (DataTypes.boole(true) )
  | "ff" => (DataTypes.boole(false) )
  | _ => (print("Enter the boolean value in the correct format");raise InputError)

  )
  val command = DataTypes.AssignmentCmd(id,symentryexp)
in
  (execute_single(command,env))
end
 )
| SOME(DataTypes.RAT_(a)) => (
let

  val inputline =(TextIO.print("Enter Decimal form for \""^a^"\" :\n");( case (TextIO.inputLine TextIO.stdIn) of 
  NONE => raise InputError
  | SOME s => s
  ))
  val num = String.substring(inputline,0,size(inputline)-1)
  val symentryexp = DataTypes.rate(Rational.fromDecimal(num)) 
  val command = DataTypes.AssignmentCmd(id,symentryexp)
in
 (execute_single(command,env))
end
 )
| _ => (print("I Hope it never comes to this\n");raise TypeError)
end
 )

 and execute_multi(cmdl : DataTypes.Cmd list,env : DataTypes.blockans ref) = 
 case cmdl of
    [] => ()
  | (x::xs) => (execute_single(x,env);execute_multi(xs,env))
end;

signature PI =


sig
    exception PiError;
    val compile : string -> DataTypes.blockans;
    val dummy : string -> DataTypes.sym;
    val interpret : string*string -> unit; 
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
fun interpret(filename,ofilename) = 
let
  val compiled = compile filename
  val env = ref compiled
  val cmds = (
    case compiled of 
    DataTypes.blockans(a,b,c,d,e) => b
    | DataTypes.Empty => []
  )
  val stream = TextIO.openOut(ofilename)
in
( Interpreter.outs := stream;Interpreter.execute_multi(cmds,env);TextIO.closeOut(stream))

end 
end;