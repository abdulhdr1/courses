(* 
* Compound types
* tuples, lists, options, records
*)

(* each of types *)

val x = { 
    foo = (true, 3),
    bar = (false, 9),
    baz = 7
}

(* type error *)
(* #h {f=3,g=12} *)

(* tuples are syntatic sugar for records *)
val a_pair = (3,4);
val a_record = {first = 3, second = 4};
val another_pair = { 1 = 3, 2 = 4};

(* datatype bindings *)
(* one of types *)
datatype mytype = TwoInts of int * int 
    | Str of string 
    | Pizza

val a = Pizza;
val b = TwoInts (12, 2);

fun f x = 
    case x of
        Pizza => 3
    |   Str s => 8
    |   TwoInts(i1, i2) => i1 + i2

val p = f Pizza
val s = f (Str "hi")
val t = f (TwoInts (p, s))

datatype suit = Club | Diamond | Heart | Spade
datatype rank = Jack | Queen | King | Ace | Num of int
datatype exp = Constant of int 
    | Negate of exp
    | Add of exp * exp
    | Multiply of exp * exp

fun eval e = 
    case e of 
        Constant i      => i
    |   Negate e2       => ~ (eval e2)
    |   Add(e1,e2)      => (eval e1) + (eval e2)
    |   Multiply(e1,e2) => (eval e1) * (eval e2)

val example_exp: exp = Add (Constant 19, Negate (Constant 4))
val example_ans: int = eval (example_exp)

fun max_constant (e: exp)   = 
    case e of 
        Constant e1       => e1
    |   Negate e1         => max_constant e1
    |   Add (e1, e2)      => Int.max(max_constant e1, max_constant e2)
    |   Multiply (e1, e2) => Int.max(max_constant e1, max_constant e2)

(* type synonyms *)

type other_pizza = Pizza
type card = suit * rank

val c1: card = (Diamond, Ace)