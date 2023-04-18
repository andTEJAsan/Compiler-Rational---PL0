
signature BIGINT = 
sig
  type bigint

  val fromInt : int -> bigint
  val fromString : string -> bigint
  val toInt : bigint -> int option
  val equals : bigint * bigint -> bool
  val add : bigint * bigint -> bigint
  val sub : bigint * bigint -> bigint
  val mul : bigint * bigint -> bigint
  val div : bigint * bigint -> bigint option
  val mod : bigint * bigint -> bigint option
  val > :   bigint * bigint -> bool
  val neg : bigint -> bigint
  val abs : bigint -> bigint
  val sign : bigint -> int
  val is_zero : bigint -> bool
  val gcd : bigint*bigint -> bigint
  val toString : bigint -> string
end
exception fool;
exception LexError;

structure BigInt : BIGINT =
struct
  (* decls *)
  type bigint = bool * (int list)
  (* to avoid multiple representations of zero we will use the default (false, [0])  *)
  fun equals(a:bigint,b:bigint) = ((#1(a) = #1(b)) andalso(#2(a)= #2(b)))
  fun fromString(s: string) = let val chrlist = explode(s) fun builder(x,y) =if (x=[]) then raise LexError else if(length(x) = 1) then(if (((ord(hd x)-48)<10)andalso(0<=(ord(hd x)-48))) then ((ord(hd x)-48)::y)else raise LexError) else if(ord(hd x)=48) then builder(tl x,y) else if(((ord(hd x)-48)<10)andalso(0<(ord(hd x)-48))) then builder(tl x,(ord(hd x)-48)::y) else raise LexError  in if(#"~" = (hd chrlist)) then let val res = builder(tl chrlist,[]) in if(res =[0]) then (false,[0]) else (true,res) end else(false, builder(chrlist,[])) end
  fun fromInt(a : int) : bigint =case a of ~1073741824 => (true,4::(#2(fromInt(107374182))))| _ => (if ( a < 0 ) then (true, #2 (fromInt(~ a))) else if (a = 0) then (false , [0]) else 
                        let
                          fun fromposint(0) = []
                          |   fromposint(a) = ( a mod 10 ) :: fromposint(a div 10)
        
                        in
                          (false, fromposint(a))
                        end)

  fun toInt((false,[x]):bigint) = SOME x
    |  toInt((true,a)) = let val x = toInt(false,a) in case x of NONE =>NONE | SOME b =>SOME (~b) end  
    |  toInt(false,a) = let val firstdig = hd a val rest = toInt(false,tl a) in 
    case rest of NONE => NONE
    |            SOME b => if(b > (valOf(Int.maxInt)-firstdig) div 10) then NONE else SOME(10*b+firstdig)
    
     end
  fun normalize(a) = let fun revnormalize([]) = [0] | revnormalize(x::xs) = case x of 0 => revnormalize(xs) | _=> x::xs  in 
      rev(revnormalize(rev(a)))  end

  fun g(i,x) = case i of 0 =>x | _ => 9::g(i-1,x)
  fun compliment(i,x) =  if x =[] then raise fool else   (*assume input is normalized unsigned int list*)
  if ( hd(x) = 0 ) then 0 :: compliment (i-1,tl x)
  else  rev (g(i-length(x),rev((10 - hd(x)) :: map (fn x => (9-x)) (tl (x)))))
  fun neg(x : bigint) : bigint = if(x = (false,[0])) then x else (not (#1 x), #2 x)
  fun abs(x : bigint) : bigint = if(#1 x) then (false,#2 x) else x
  fun sign(x : bigint) = case x of (false,[0]) => 0 | (false,_) => 1 | (true,_) => ~1 
  fun toString(x : bigint) = case x of (true,_) =>"~"^toString(false,#2 x) | (false,[a]) => Int.toString(a) | _=>toString(false,tl (#2 x)) ^ Int.toString(hd(# 2 x) )
  fun uadd(x : int list , y : int list,c : int ) : int list  = (* normalized int list unsigned*)
  if length(y) > length(x) then uadd(y,x,c) else 
  if (x =[]) then if (c = 0 ) then y else uadd([c],y,0) else
  if (y = []) then if (c = 0) then x else uadd(x,[c],0) else
  let
    val firstdig = ((hd x) + (hd y) + c ) mod 10
    val c = ((hd x) + (hd y) +c ) div 10
    val remaining = uadd(tl x, tl y , c)
  in
    firstdig :: remaining
  end
  fun add((true,x):bigint,(true,y):bigint):bigint = (true,uadd(x,y,0))
  |   add((false,x),(false,y)) = (false,uadd(x,y,0))
  |   add((false,x),(true,y)) = 
      let
          val len = if(length(x)>length(y)) then length(x) else length(y)
          val compl = compliment(len,y)
          val sum = uadd(x,normalize(compl),0)
          val ans = if(length(sum) =(len+1) ) then (false,normalize(List.take(sum,len))) else (true,normalize(compliment(len,sum)))
      in
        ans
      end
  |   add((true,x),(false,y)) = add((false,y),(true,x))

  fun sub(x,y) = add(x,neg(y))
  fun karatsuba(x : int list ,y : int list) : int list = (*normalized unsigned int*)
  if (length(x) = 1 andalso length(y) = 1) then #2 (fromInt((hd x)* (hd y)))
  else if (length(y) > length(x)) then karatsuba(y,x) else
  let
    val len = length(x) div 2
    val maxl = if(len > length(y)) then length(y) else len
    val y0 = normalize(List.take(y,maxl))
    val y1 = List.drop(y,maxl)
    val x0 = normalize(List.take(x,len))
    val x1 = List.drop(x,len)
    val z0 = karatsuba(x0,y0)
    fun f(i,x) = case i of 0 => x | _=> 0::f(i-1,x)
  in
    if(y1 = []) then uadd(z0,normalize(f(len,karatsuba(x1,y0))),0) else let
      val z2 = karatsuba(x1,y1)
      val z1 = #2 (add((true,uadd(z0,z2,0)),(false,karatsuba(uadd(x1,x0,0),uadd(y1,y0,0)))))
      val z3 = normalize(f(len,z1))
      val z4 = normalize(f(2*len,z2))
      val an = uadd(z0,z4,0)
      val ans = uadd(an,z3,0)

    in
      ans
    end

  end

  fun mul((false,x):bigint,(false,y):bigint) :bigint = (false,karatsuba(x,y))
  |   mul((true,x),(false,y)) = let val ans = karatsuba(x,y) in if(ans =[0]) then (false,[0]) else (true,ans) end
  |   mul((false,x),(true,y)) = let val ans = karatsuba(x,y) in if(ans =[0]) then (false,[0]) else (true,ans) end
  |   mul((true,x),(true,y)) = (false,karatsuba(x,y))


  fun udiv(a : int list , b : int list ) : (int list)*(int list) = 
  let
    
    fun divider(c : int list) = let fun g(i) =  if((i = 10 ) orelse (sign((sub((false,c),(false,karatsuba([i],b))))) = ~1)) then (i-1,#2 (sub((false,c),(false,karatsuba([i-1],b))))) else g(i+1) in g(0) end
    
    fun udivhelper(R,Q,RD) = if RD = [] then (R,Q)
    else 
    let
      val dividend = normalize(hd(RD)::R)
      val (newdig,newrem) = divider(dividend)
    in
      udivhelper(newrem,normalize(newdig::Q),tl RD)
    end
    
  in
    udivhelper([],[],rev(a))
  end
  fun pdiv((false,x),(false,y)) = (false,#2 (udiv(x,y)))
  |    pdiv((true,x),(true,y)) = (false,#2 (udiv(x,y)))
  |    pdiv((false,x),(true,y)) = let val quot = udiv(x,y) in if(#1(quot) = [0]) then if(#2(quot) = [0]) then (false,[0]) else (true,#2(quot)) else(true,uadd([1],#2(quot),0)) end
  |    pdiv((true,x),(false,y)) = pdiv((false,x),(true,y))

  fun mdiv(a: bigint,b:bigint) :bigint= let
    val remainder = udiv(#2(a),#2(b))
    val x = #1 (a)
    val y = #1 (b)
  in
    if #1(remainder) = [0] then(false,[0]) else 
    case (x,y) of 
    (false,false) => (false,#1(remainder))
    |(true,true) =>   (true,#1(remainder))
    |(true,false) => (false,uadd([1],#1(remainder),0))
    |(false,true) => add((false,#1(remainder)),b)

  end
 
  fun op div(x : bigint,y:bigint): bigint option = if (y = (false,[0])) then NONE else SOME (pdiv(x,y))
  fun op mod(x:bigint,y:bigint) : bigint option = if (y = (false,[0])) then NONE else SOME (mdiv(x,y))
  fun op > (x: bigint,y: bigint) : bool = (sign(sub(x,y)) = 1 );
  fun is_zero(x : bigint) : bool = if( x = (false,[0])) then true else false
  fun gcd(x : bigint,y : bigint) : bigint =  (* gcd of unsigned positive integers*)
  
  if(sign(x) = ~1) then gcd(neg(x) , y ) else
  if ( is_zero(x)) then y else
  if (is_zero(y)) then x else gcd(y, valOf(x mod y))



end

signature RATIONAL =
sig
type bigint
type rational
exception rat_errorval
exception zero_division_error 
val make_rat: bigint * bigint -> rational option (**)
val rat: bigint -> rational option (**)
val reci: bigint -> rational option (**)
val neg: rational -> rational (**)
val inverse : rational -> rational option (**)
val equal : rational * rational -> bool (* equality *) (**)
val less : rational * rational -> bool (* less than *) (**)
val add : rational * rational -> rational (* addition *) (**)
val subtract : rational * rational -> rational (* subtraction *)(**)
val multiply : rational * rational -> rational (* multiplication *) (**)
val divide : rational * rational -> rational option (* division *) (**)
val showRat : rational -> string (**)
val showDecimal : rational -> string (**)
val fromDecimal : string -> rational (**)
val toDecimal : rational -> string (**)
end;
functor rationalizer (BigInt : BIGINT) :RATIONAL =
struct
  type rational = (BigInt.bigint) * (BigInt.bigint)
  type bigint = (BigInt.bigint)
  exception rat_errorval
  exception zero_division_error
  fun make_rat(a:bigint,b:bigint):rational option = if (BigInt.is_zero(b)) then NONE else
  let
    fun make_rat_helper(x,y) = if(BigInt.sign(y) = ~1) then make_rat_helper(BigInt.neg(x),BigInt.neg(y)) else  (valOf(BigInt.div(x,BigInt.gcd(x,y))),valOf(BigInt.div(y,BigInt.gcd(x,y))))
  in
    SOME (make_rat_helper(a,b))
  end
  fun rat(a) = make_rat(a,BigInt.fromInt(1))
  fun reci( a : bigint) : rational option = make_rat(BigInt.fromInt(1),a)
  fun neg((x,y) : rational) = (BigInt.neg(x),y)
  fun inverse((x,y) : rational) : rational option = if (BigInt.is_zero(x)) then NONE else SOME(if(BigInt.sign(x) = ~1) then (BigInt.neg(y),BigInt.neg(x)) else (y,x))
  
  fun decihelper((x,y):rational) = let fun h(a ,l1: (BigInt.bigint) list,l2) = let
    val remainder =  valOf (BigInt.mod(a,y))
  fun find(l : (BigInt.bigint) list,x : BigInt.bigint) : int = let fun finder(a:(BigInt.bigint) list,y: BigInt.bigint,i : int) = if(length(a) = 0) then i else if(BigInt.sign(BigInt.sub(hd a, y)) = 0) then i else finder(tl a,y,i+1) in finder(l,x,0) end

    val idx = find(l1,remainder) 
    val nextdiv = BigInt.mul(BigInt.fromInt(10),remainder)
    val divided = valOf(BigInt.div(a,y))  
  
   in if(idx = length l1) then h(nextdiv,remainder :: l1,divided::l2) else
   
   
   let val r = rev( divided::l2)
        val index = (length (l1) - idx -1) 
        val l3 =List.take(r,index+1)
        val l4 = List.drop(r,index+1)  in "."^String.extract(implode(map(fn x=> chr(x+48))(map(fn x=> valOf(BigInt.toInt(x))) l3)),1,NONE)^"("^implode(map(fn x=> chr(x+48)) (map(fn x=> valOf(BigInt.toInt(x))) l4))^")" end

     end
  
  
  
  in h(x,[],[]) end
  fun dh((x,y):rational) = BigInt.toString(valOf(BigInt.div(x,y)))^decihelper((x,y))
  
  fun showDecimal((x,y): rational) = if (BigInt.sign(x) = ~1 andalso BigInt.sign(y) = ~1 ) then showDecimal((BigInt.neg(x),BigInt.neg(y)))
                                      else if (BigInt.sign(x) = 1 andalso BigInt.sign(y) = ~1 ) then "~"^showDecimal((x,BigInt.neg(y)))
                                      else if (BigInt.sign(x) = ~1 andalso BigInt.sign(y) = 1 ) then "~"^showDecimal(BigInt.neg(x),y)
                                      else dh((x,y))
  fun showRat((x,y) : rational) : string = BigInt.toString(x)^"/"^BigInt.toString(y) 
  fun equal((x,y):rational,(a,b):rational) = (BigInt.equals(#1(valOf (make_rat(a,b))),#1(valOf (make_rat(x,y))))andalso(BigInt.equals(#2(valOf (make_rat(a,b))),#2(valOf (make_rat(x,y))))))
  fun less((x,y):rational,(a,b):rational) = (BigInt.sign(BigInt.sub(BigInt.mul(x,b),BigInt.mul(a,y))) = ~1)
  fun add((x,y):rational,(a,b):rational) = valOf(make_rat(BigInt.add(BigInt.mul(a,y),BigInt.mul(x,b)),BigInt.mul(y,b)))
  fun multiply((x,y):rational,(a,b):rational) = valOf(make_rat(BigInt.mul(a,x),BigInt.mul(b,y)))
  fun subtract(a,b) = add(a,neg(b))
  fun divide((x,y):rational,(a,b):rational) = if(
    BigInt.is_zero(a)
  ) then raise zero_division_error
  else SOME (valOf(make_rat(BigInt.mul(b,x),BigInt.mul(a,y))))

  fun k(a) = if(length(a) < 2) then raise LexError else if(List.last(a) <> #")") then raise LexError else let val num = String.extract(implode(a),0,SOME(length(a)-1)) val den = implode(let fun bo(m) = (if(m=0) then[] else (#"9")::bo(m-1)) in bo(length(a)-1) end) in valOf(make_rat(BigInt.fromString(num),BigInt.fromString(den))) end 
  fun h(a : char list) : rational = if (length(a)=0) then valOf(rat(BigInt.fromInt(0))) else valOf (rat(BigInt.fromString(implode(rev(a)))))
  fun g(a : char list) : rational = let fun oi(a,b:rational,acc:rational) =if(a=[]) then raise LexError else if (hd(a) = #"(") then add(acc,multiply(b,k(tl(a)))) else  if(((ord(hd a)-48)<10)andalso(0<=(ord(hd a)-48))) then  (let val next = multiply(b,valOf(reci(BigInt.fromInt(10)))) val nextacc = add(acc,multiply(next,valOf(rat(BigInt.fromInt(ord(hd(a))-48))))) in oi(tl a,next,nextacc) end) else raise LexError in oi(a,valOf(rat(BigInt.fromInt(1))),valOf(rat(BigInt.fromInt(0)))) end
  fun fromCharlist(x,y) =if(x=[]) then raise LexError else if(hd (x) = #"~") then neg(fromCharlist(tl (x),y)) else
                        if(hd(x) = #"+") then fromCharlist(tl x,y) else
                        if(hd(x) = #".") then add(h(y), g(tl x)) else 
                        if(((ord(hd x)-48)<10)andalso(0<=(ord(hd x)-48))) then fromCharlist( tl x, hd(x) :: y) else raise LexError


                        
  fun fromDecimal(a:string) = fromCharlist(explode(a),[])
  val toDecimal = showDecimal

end
structure Rational = rationalizer(BigInt)
