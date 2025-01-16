(*  date = int *  int  * int *)
(*        year * month * day *)
(* year - positive *)
(* month - 1-12 *)
(* day - >31 *)
(* day of year - 1-365 *)

type date = int * int * int

fun date_year(date:date) = #1 date
fun date_month(date:date) = #2 date
fun date_day(date:date) = #3 date

fun is_older (x: date, y: int * int * int) =
  let
    val year_x = date_year x
    val year_y = date_year y
    val month_x = date_month x
    val month_y = date_month y
    val day_x = date_day x
    val day_y = date_day y
    val same_year = year_x = year_y
    val same_month = month_x = month_y
  in
    if year_x < year_y then
      true
    else
      if same_year then
        if month_x < month_y then true
        else
          if same_month then day_x < day_y
          else false
      else false
  end;

(* Write a function number_in_month that takes a list of dates and a month (i.e., an int) and returns
how many dates in the list are in the given month. *)
fun number_in_month (dates_list: date list, month: int) =
  let 
    fun process_date (date: date) = if (date_month date) = month then 1 else 0 
    fun process_dates_list (dates_list: date list, count: int) = 
      if null dates_list
      then count
      else 
        let val new_count = count + (process_date (hd dates_list)) 
        in process_dates_list(tl dates_list, new_count)
        end 
  in process_dates_list (dates_list, 0) 
  end

  
(* Write a function number_in_months that takes a list of dates and a list of months (i.e., an int list)
and returns the number of dates in the list of dates that are in any of the months in the list of months.
Assume the list of months has no number repeated. Hint: Use your answer to the previous problem. *)
fun number_in_months (dates_list: date list, months_list: int list ) = 
  let 
    fun count_in_month (month: int) = number_in_month (dates_list, month)
    fun process_months_list (months_list: int list, count: int) = 
      if null months_list
      then count
      else (count_in_month (hd months_list) + process_months_list (tl months_list, count))
  in process_months_list (months_list, 0)
  end

(* Write a function dates_in_month that takes a list of dates and a month (i.e., an int) and returns a
list holding the dates from the argument list of dates that are in the month. The returned list should
contain dates in the order they were originally given. *)
fun dates_in_month (dates_list: date list, month: int) =
  if null dates_list
  then []
  else 
    let 
      fun is_date_in_month (date: date) = (date_month date) = month
      val next = dates_in_month(tl dates_list, month)
    in
      if is_date_in_month (hd dates_list)
      then (hd dates_list)::next
      else next
    end

(* Write a function dates_in_months that takes a list of dates and a list of months (i.e., an int list)
and returns a list holding the dates from the argument list of dates that are in any of the months in
the list of months. Assume the list of months has no number repeated. Hint: Use your answer to the
previous problem and SML’s list-append operator (@). *)
fun dates_in_months (dates_list: date list, months_list: int list) =
  if null months_list
  then []
  else (dates_in_month (dates_list, hd months_list))@(dates_in_months (dates_list, tl months_list))

(* Write a function get_nth that takes a list of strings and an int n and returns the n
th element of the list where the head of the list is 1st. Do not worry about the case where 
the list has too few elements: your function may apply hd or tl to the empty list in this case, which is okay. *)
fun get_nth(string_list: string list, n: int) =
  if n = 1 
  then hd string_list
  else get_nth(tl string_list, n-1)
(* Not sure what the last of the description part means, I'm using hd and tl and when applying them to an empty list
ie hd [] or tl [] I still get nil, which errors because of diverging return types *)


(* Write a function date_to_string that takes a date and returns a string of the form January 20, 2013
(for example). Use the operator ^ for concatenating strings and the library function Int.toString
for converting an int to a string. For producing the month part, do not use a bunch of conditionals.
Instead, use a list holding 12 strings and your answer to the previous problem. For consistency, put a
comma following the day and use capitalized English month names: January, February, March, April,
May, June, July, August, September, October, November, December. *)
fun date_to_string (date: date ) = 
  let 
    val month_string_list = ["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"]
    val month_string = get_nth(month_string_list, date_month date) 
    val day_string = Int.toString(date_day date) 
    val year_string = Int.toString(date_year date) 
  in month_string ^ " " ^ day_string ^ ", " ^ year_string 
  end

