  (* straight insertion sort pg 80/81 *)

let records = [503;087;512;061;908;170;897;275;653;426;154;509;612;677;765;703];;

let rec insert_element_into_sorted_list e list = match list with
  | [] -> e :: list
  | h :: t -> if e <= h then e :: list
	      else h :: (insert_element_into_sorted_list e t);;
		
 
let straight_insertion_sort list =
  let rec aux list sorted_list = match list, sorted_list with
    | [] , _ -> sorted_list
    | h1 :: t1 , []  -> aux t1 (h1 :: sorted_list)
    | h1 :: t1 , h2 :: t2 -> if h1 <= h2 then aux t1 (h1 :: sorted_list)
			     else aux t1 (insert_element_into_sorted_list h1 sorted_list) 
  in aux list [] ;;

(* Shell sort / Shell's method / Diminishing increment sort pg 83/84 *)
(* (R1, R9) (R2, R10)... (R8, R16) *)

let rec get_kth_element_from_list list k = match k, list with
  | 1, h :: t -> h
  | _, h :: t-> get_kth_element_from_list t (k-1)

let rec length list = match list with
  | [] -> 0
  | h :: t -> 1 + (length t)

let rec get_rid_of_first_m_elements list m =
  if (length list) < m then []
  else match m, list with
       | 0 , _ -> list
       | _ , h :: t -> get_rid_of_first_m_elements t (m-1);;  
    		 		
let rec find_all_elements_in_list_that_are_0_mod_k_apart_from_first_element list k =  match list with
    | [] -> []
    | h :: t -> h :: (find_all_elements_in_list_that_are_0_mod_k_apart_from_first_element (get_rid_of_first_m_elements t (k-1)) k)

let rec flatten list = match list with
  | [] -> []
  | h :: t -> h @ (flatten t)

let rec append_element element list = match list with
  | [] -> [element]
  | h :: t -> h :: append_element element t
  
let divide_into_groups list d =
  let stop = length (list) in
  let rec aux list return_list = match (length(flatten (return_list)) = stop ), list with
  | _ , [] -> return_list
  | true , _ -> return_list		
  |_ , h :: t -> aux t (append_element (find_all_elements_in_list_that_are_0_mod_k_apart_from_first_element list d) return_list)
  in aux list []

let rec map f list = match list with
  | [] -> []
  | h ::t -> f h :: map f t

let shell_sort list a b c =
  let first_pass = flatten (map straight_insertion_sort (divide_into_groups list a)) in
  let second_pass = flatten (map straight_insertion_sort (divide_into_groups first_pass b)) in
  let third_pass = flatten (map straight_insertion_sort (divide_into_groups second_pass c)) in
  flatten (map straight_insertion_sort (divide_into_groups third_pass 1))
    
  
  


