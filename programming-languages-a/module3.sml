(* Nested functions *)

fun countup_from1(x: int) = 
        let 
            fun count (from: int) =   
                    if from=x
                    then x::[]
                    else from :: count(from+1)
        in 
            count (1) 
        end;

countup_from1(10);

(* Let and efficiency *)

fun bad_max(xs: int list ) =
    if null xs 
    then 0
    else if null (tl xs)
    then hd xs
    else if hd xs > bad_max(tl xs)
    then hd xs
    else bad_max(tl xs);

fun good_max (xs: int list) = 
    if null xs
    then 0
    else if null (tl xs)
    then hd xs
    else 
        let val tl_ans = good_max(tl xs)
        in 
            if hd xs > tl_ans
            then hd xs
            else tl_ans
        end


(* Options *)

(* fn: int list -> int option *)
fun max1 (xs: int list) = 
    if null xs 
    then NONE
    else 
        let val tl_ans = max1(tl xs)
        in if isSome tl_ans andalso valOf tl_ans > hd xs
            then tl_ans
            else SOME (hd xs)
        end

fun max2 (xs:int list) = 
    if null xs
    then NONE
    else 
        let fun max_nonempty (xs: int list) = 
            if null (tl xs)
            then hd xs
            else let val tl_ans = max_nonempty (tl xs)
                in 
                    if hd xs > tl_ans
                    then hd xs
                    else tl_ans
                end
        in
            SOME (max_nonempty xs)
        end

(* Booleans and comparison operations *)

(* 
e1 andalso e2;  
e1 orelse e2;

= equals
<> not equals
*)

(* Benefits of no mutation *)

(* 
when you have mutable state you can't tell if things
are aliases or copies of data

the lack of mutations means there's no difference 
between copies and aliases/references, making them safer  
*)

(* Pieces of learning a language *)

(* 
1. syntax       - how to write x 
2. semantics    - what do programs means, how is x evaluated 
3. idioms       - when to use (patterns of use) x
4. libraries
5. tools
*)
