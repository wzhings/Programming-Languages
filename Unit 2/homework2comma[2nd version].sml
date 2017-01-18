(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
  s1 = s2

(* 1-(a)*)
fun all_except_option (str, str_list) =
  case str_list of
      [] => NONE
    | head::tail => if same_string(str, head)
		    then
			SOME tail
		    else
			case all_except_option(str,tail) of
			    NONE => NONE
			  | SOME xs => SOME (head::xs) 
(* We need this, because we also need to output other strings on "head" *)

(* 1-(b) *)
fun get_substitutions1 (str_llists, str) =
  case str_llists of
      [] => []
    | head :: tail => case all_except_option(str,head) of
			  NONE => get_substitutions1(tail,str) (* modify 1st*)
			| SOME xs => xs @ get_substitutions1(tail,str)
							  
(* 1-(c) *)
fun get_substitutions2 (str_llists, str) =
  let fun aux(str_llists, acc) =
	case str_llists of
	    [] => acc
	  | head :: tail => case all_except_option(str,head) of
				NONE => aux(tail, acc)
			      | SOME xs => aux(tail, acc @ xs)
  in
      aux(str_llists,[])
  end

(* 1-(d) *)
fun similar_names (str_llist, name) =
  let
      val {first=x, middle=y, last=z} = name
      fun aux reslt =
	case reslt of
	    [] => []
	  | head::tail => {first=head, middle=y,last=z}::aux(tail)
  in
      name::aux(get_substitutions2(str_llist, x))
  end
			     
(* put your solutions for problem 1 here *)

(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

(* put your solutions for problem 2 here *)
	      
(* 2-(a) *)
fun card_color card =
  case card of
      (Clubs,_) => Black
    | (Spades,_) => Black
    | (Hearts,_) => Red
    | (Diamonds,_) => Red 
				 
(* 2-(b) *)
fun card_value card =
  case card of
      (_,Jack) => 10
    | (_,Queen) => 10
    | (_,King) => 10
    | (_,Ace) => 11
    | (_,Num n) => n 
    
		 
(* 2-(c) *)
fun remove_card (cs, c, ex) =
  case cs of
      [] => raise ex
    | x::xs => if x = c
	       then xs
	       else x::remove_card (xs, c, ex) (* Don't forget " head::" *)
				
(* 2-(d) *)
fun all_same_color list_card =
  case list_card of
      [] => true
     |[_] => true
     | head::neck::rest => card_color head = card_color neck andalso all_same_color(neck::rest)  (* Please remember this method *)

(* 2-(e) *)
fun sum_cards list_card =
  let fun aux(list, acc) =
	case list of
	    [] => acc
	  | x::xs => aux(xs, acc+card_value(x))
  in
      aux(list_card, 0)
  end

(* 2-(f)*)
fun score (cs, num) =
  let val sre = sum_cards(cs)
  in
      if all_same_color(cs)
      then if sre > num
	   then 3 * (sre - num) div 2
	   else (num - sre) div 2
      else
	  if sre > num
	  then 3 * (sre - num)
	  else num - sre
  end
      
(* 2-(g)*)
fun officiate (cs, ml, num) =
  let fun aux(card_list, move_list, held_card) =
	case move_list of
	    [] => score(held_card, num)
	  | (Discard x)::rest => aux(card_list, rest, remove_card(held_card, x, IllegalMove))
	  | Draw::rest => case card_list of
			      [] => score(held_card, num)
			    | head::tail => if sum_cards(head::held_card) > num
					    then score(head::held_card, num)
					    else aux(tail, rest, head::held_card)
  in
      aux(cs, ml, [])
  end
      
(* I have not finished the challenged problems *)  
						   
						   
				   
			
				   
  
	  
	
  
  


	      
