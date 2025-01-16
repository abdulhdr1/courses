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