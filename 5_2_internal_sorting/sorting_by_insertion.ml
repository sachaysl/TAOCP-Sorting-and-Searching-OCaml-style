(* Straight Insertion pg 80/81 *)


let rec insert_element_into_sorted_list e list = match list with
  | [] -> e :: list
  | h :: t -> if e <= h then e :: list
	      else h :: (insert_element_into_sorted_list e t)
		
 
let straight_insertion_sort list =
  let rec aux list sorted_list = match list, sorted_list with
    | [] , _ -> sorted_list
    | h1 :: t1 , []  -> aux t1 (h1 :: sorted_list)
    | h1 :: t1 , h2 :: t2 -> if h1 <= h2 then aux t1 (h1 :: sorted_list)
			     else aux t1 (insert_element_into_sorted_list h1 sorted_list) 
  in aux list [];;
  
