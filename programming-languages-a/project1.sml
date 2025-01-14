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

  
