use "project1.sml";
(* Homework1 Simple Test *)
(* These are basic test cases. Passing these tests does not guarantee that your code will pass the actual homework grader *)
(* To run the test, add a new line to the top of this file: use "homeworkname.sml"; *)
(* All the tests should evaluate to true. For example, the REPL should say: val test1 = true : bool *)
(* val test2 = number_in_month ([(2012,2,28),(2013,12,1)],2) = 1 *)
(* val test3 = number_in_months ([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[2,3,4]) = 3 *)
(* val test4 = dates_in_month ([(2012,2,28),(2013,12,1)],2) = [(2012,2,28)] *)
(* val test5 = dates_in_months ([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[2,3,4]) = [(2012,2,28),(2011,3,31),(2011,4,28)] *)
(* val test6 = get_nth (["hi", "there", "how", "are", "you"], 2) = "there" *)
(* val test7 = date_to_string (2013, 6, 1) = "June 1, 2013" *)
(* val test8 = number_before_reaching_sum (10, [1,2,3,4,5]) = 3 *)
(* val test9 = what_month 70 = 3 *)
(* val test10 = month_range (31, 34) = [1,2,2,2] *)
(* val test11 = oldest([(2012,2,28),(2011,3,31),(2011,4,28)]) = SOME (2011,3,31) *)
(* val inf = 999; *)

val base = (1,1,1)
val base_month = date_month base

val test1_1   = is_older (base,   base) = false
val test1_2   = is_older (base, (9,1,1))= true
val test1_3   = is_older ((9,1,1),base) = false
val test1_4   = is_older ((9,9,1),base) = false
val test1_5   = is_older ((9,9,9),base) = false
val test1_6   = is_older (base,(9,1,1)) = true
val test1_7   = is_older (base,(9,9,1)) = true
val test1_8   = is_older (base,(9,9,9)) = true
val test1_9   = is_older (base,(1,1,9)) = true
val test1_11  = is_older (base,(1,9,1)) = true
val test1_13  = is_older (base,(9,1,1)) = true
val test1_10  = is_older ((1,1,9),base) = false
val test1_12  = is_older ((1,9,1),base) = false
val test1_14  = is_older ((9,1,1),base) = false

val test2_1 = number_in_month ([(2012,2,28), (2013,12,1)],2)             = 1
val test2_2 = number_in_month ([(2012,2,28), (2013,12,1)],2)             = 1
val test2_3 = number_in_month ([],           1)                          = 0
val test2_4 = number_in_month ([(2012,1,1)], 1)                          = 1
val test2_5 = number_in_month ([(2012,1,1),  (2012,1,2)], 1)             = 2
val test2_6 = number_in_month ([(2012,1,1),  (2012,2,1)], 1)             = 1
val test2_7 = number_in_month ([(2012,2,1),  (2012,3,1)], 1)             = 0
val test2_8 = number_in_month ([(2012,1,1),  (2012,1,2), (2012,2,1)], 1) = 2

val test3_1 = number_in_months ([(2012,2,28), (2013,12,1),(2011,3,31),(2011,4,28)],[2,3,4]) = 3
val test3_2 = number_in_months ([],           [1,2,3])                                      = 0
val test3_3 = number_in_months ([(2012,1,1)], [])                                           = 0
val test3_4 = number_in_months ([(2012,1,1)], [1])                                          = 1
val test3_5 = number_in_months ([(2012,1,1), (2012,2,1)], [1,2])                            = 2
val test3_6 = number_in_months ([(2012,1,1), (2012,2,1)], [3,4])                            = 0
val test3_7 = number_in_months ([(2012,1,1), (2012,1,2)], [1,2])                            = 2
val test3_8 = number_in_months ([(2012,1,1), (2012,2,2), (2012,3,3)], [1,2,3])              = 3

val test4_1 = dates_in_month ([base,     (2013,12,1)],          base_month) = [base]
val test4_2 = dates_in_month ([base,     (2013,12,1), base],    base_month) = [base, base]
val test4_3 = dates_in_month ([(99,2,99),(2013,2,1),  base],    2)          = [(99,2,99),(2013,2,1)]
val test4_4 = dates_in_month ([base,     base,        base],    9)          = []
val test4_5 = dates_in_month ([(1,9,2),  (1,9,3),     (1,9,4)], 9)          = [(1,9,2),(1,9,3),(1,9,4)]

val test5_1 = dates_in_months ([
    (2012,2,28),
    (2013,12,1),
    (2011,3,31),    
    (2011,4,28)], 
    [2,3,4]) = [(2012,2,28),(2011,3,31),(2011,4,28)]
val test5_2 = dates_in_months ([
    base,
    base,
    (2011,3,31),
    (2011,4,28)], 
    [base_month,3,4]) = [base, base,(2011,3,31),(2011,4,28)]
val test5_3 = dates_in_months ([
    (2011,3,31),
    (2011,4,28)],
    [3,4]) = [(2011,3,31),(2011,4,28)]
val test5_4 = dates_in_months ([
    (2012,2,28),
    base,
    (2011,3,31),
    (2011,4,28)],
    [base_month]) = [base]
val test5_5 = dates_in_months ([
    (2012,2,28),
    (2013,12,1),
    (2011,3,31),
    (2011,4,28)],
    []) = []
