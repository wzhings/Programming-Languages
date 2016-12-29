(* HW1 - 1: is_older function *)
fun is_older (d1 : int*int*int, d2 : int*int*int) =
  if #1 d1 < #1 d2
  then true
  else if #1 d1 > #1 d2
  then false
  else
      if #2 d1 < #2 d2
      then true
      else if #2 d1 > #2 d2
      then false
      else
	  if #3 d1 < #3 d2
	  then true
	  else false

(* HW1 - 2 : number_in_month *)
fun number_in_month (dalist : (int*int*int) list, mon : int) =
  if null dalist
  then 0
  else
      if #2 (hd (dalist)) = mon
      then 1 + number_in_month (tl(dalist), mon)
      else number_in_month (tl(dalist), mon)

(* HW1 - 3 : number_in_months *)
fun number_in_months (dalist : (int*int*int) list, mons : int list) =
  if null dalist orelse null mons
  then 0
  else
      let
	  val num_ans = number_in_month(dalist, hd mons)
      in
	  if null (tl mons)
	  then num_ans
	  else num_ans + number_in_months (dalist, tl mons)
      end		

(* HW1 - 4 : dates_in_month *)
fun dates_in_month (dalist : (int*int*int) list, mon : int) =
  if null dalist
  then []
  else
      if #2 (hd (dalist)) = mon
      then hd dalist :: dates_in_month (tl dalist, mon)
      else dates_in_month (tl dalist, mon)
	  
(* HW1 - 5 : dates_in_months *)      
fun dates_in_months (dalist : (int*int*int) list, mons : int list) =
  if null dalist orelse null mons
  then []
  else
      let val mon_ans = dates_in_month(dalist, hd mons)
      in
	  if null (tl mons)
	  then mon_ans
	  else mon_ans @ dates_in_months(dalist, tl mons)
      end
(* HW1 - 6: get_nth *)
fun get_nth (strl : string list, num : int) =
  if null strl
  then ""
  else
      if num = 1
      then hd strl
      else get_nth (tl strl, num - 1)	      
	      
      
(* HW1 - 7: date_to_string *)
fun date_to_string (da : (int*int*int)) =
  let
      val months = ["January ","February ","March ","April ","May ","June ","July ","August ","September ","October ","November ","Decemeber "];
  in
      get_nth(months, #2 da)^Int.toString(#3 da)^", "^Int.toString(#1 da)
  end
      
(* HW1 - 8 : number_before_reaching_sum *)
fun number_before_reaching_sum (sum : int, numl : int list) =
  if sum <= hd numl
  then 0
  else 1 + number_before_reaching_sum (sum - hd numl, tl numl)
(* this is the way to return the number, do not need count variables *)

				      
(* HW1 - 9 : what_month *)
fun what_month (day : int) =
  let
      val months = [31,28,31,30,31,30,31,31,30,31,30,31]
  in
      1 + number_before_reaching_sum (day, months)
  end
      

(* HW1 - 10 : month_range *)
fun month_range (day1 : int, day2 : int) =
  if day1 > day2
  then []
  else what_month day1 :: month_range (day1 + 1, day2)
				      
(* HW1 - 11 : oldest *)
fun oldest (dalist : (int*int*int) list) =
  if null dalist
  then NONE
  else
      let
	  val older_mn = oldest (tl dalist)
      in
	  (* myself_wrong: if is_older(hd dalist, valOf(older_mn)) *)
	  if isSome older_mn andalso is_older(valOf older_mn, hd dalist)
	  then older_mn
	  else SOME (hd dalist)
      end
	  
	   

      
  
  
