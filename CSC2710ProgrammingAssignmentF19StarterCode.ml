(* CSC 2710 starter code for breadth-first bottom-up parser implemented in OCaml
   Author: Martha Kosa
   Date: 11.19.2019 *)

(* Modifications by Maddison Davenport, Josh Schonoover and Hannah Lafever*)

(****************************)
(* list utility functions *)

(* converts from String to list of chars *)
(* from https://caml.inria.fr/mantis/view.php?id=5367 *)
let explode s =
  let rec exp i l =
    if i < 0 then l else exp (i - 1) (s.[i] :: l) in
    exp (String.length s - 1) [];;

(* TODO: !!!!!DONE!!!!! Call explode with your name as input.  If you are working in a team, call
   explode once for each team member. *)

let maddison = explode "Maddison";;
let josh = explode "Josh";;
let hannah = explode "Hannah";;

(* converts from list of chars to String *)
(* single char conversion from https://stackoverflow.com/questions/20441263/convert-char-to-string-in-ocaml/20463186 *)
let rec to_one_string char_list =
  match char_list with
    | [] -> ""
    | h::t -> (String.make 1 h)^(to_one_string t);; (* ^ does string concatenation *)

(* TODO: !!!!!DONE!!!!! Call to_one_string with the result(s) of your call(s) to explode. *)

to_one_string maddison;;
to_one_string josh;;
to_one_string hannah;;

(* tail-recursive list reversal *)
let reverse xs =
  let rec reverse_tail xs rxs =
    match xs with
      | [] -> rxs
      | h::t -> reverse_tail t (h::rxs)
  in reverse_tail xs [];;

(* TODO: !!!!!DONE!!!!! Call reverse with the result(s) of your call(s) to explode. *)

reverse maddison;;
reverse josh;;
reverse hannah;;

(* TODO: !!!!!DONE!!!!!Complete (and uncomment) the below recursive function to determine whether a list is a prefix of another list. *)
let rec prefix xs ys =
  match (xs, ys) with
    | ([], _) -> true
    | (_, []) -> false
    | (x::xss, y::yss) -> 
        if (x = y) then prefix xss yss else false;;

(* TODO: !!!!!DONE!!!!!Write 8 test cases to test your prefix function thoroughly. *)

let test1 = [1; 2; 3];;
let test2 = [1; 2; 3; 4];;
let test3 = [5; 4; 3; 2];;
let test4 = [5; 4; 3];;
let test5 = [1; 2];;


prefix test1 test2;;
prefix test2 test3;;
prefix test1 test3;;
prefix test1 test4;;
prefix test2 test4;;
prefix test4 test3;;
prefix test5 test1;;
prefix test5 test2;;


(* TODO: !!!!!DONE!!!!!Complete (and uncomment) the below function to determine whether
   a list is a suffix of another list. *)
let suffix xs ys = prefix (reverse xs) (reverse ys);;

(* TODO: Write 8 test cases to test your suffix function thorougly. *)

let test6 = [1; 2; 3; 4; 5];;
let test7 = [3; 4; 5];;
let test8 = [4; 3; 2; 1];;
let test9 = [2; 3; 4; 5];;

suffix test7 test6;;
suffix test6 test7;;
suffix test9 test6;;
suffix test8 test1;;
suffix test5 test6;;
suffix test7 test9;;
suffix test1 test2;;
suffix test6 test7;;


(* TODO: !!!!!DONE!!!!! Write a comment to explain what map does. *)
(* Map takes a function and a list as parameters, then applies the function to every item in the list *)
let rec map f xs =
  match xs with
    | [] -> []
    | h::t -> (f h)::(map f t);;

(* TODO: !!!!!DONE!!!!! Write a comment to explain what filter does. *)
(* Takes a list of items and filters it through the function and keeps the items that are true in the function*)
let rec filter f xs =
  match xs with
    | [] -> []
    | h::t ->
        if (f h) then h::(filter f t) else (filter f t);;

let get_all_nonempty_prefixes xs =
  let rec get_all_nonempty_prefixes_tr xs last acc =
    match xs with
      | [] -> acc
      | h::t -> let next = last@[h] in
            get_all_nonempty_prefixes_tr t next ([next]@acc)
  in reverse (get_all_nonempty_prefixes_tr xs [] []);;

(* TODO: !!!!!DONE!!!!!Write 2 nontrivial test cases to test get_all_non_empty_prefixes thoroughly. *)

get_all_nonempty_prefixes [1; 2; 3; 4; 0; 5];;
get_all_nonempty_prefixes ["Maddi"; "Hannah"; "Josh"; "Martha"]

(* TODO: !!!!!DONE!!!!!Write 2 nontrivial test cases to test get_all_non_empty_suffixes thoroughly. *)

let get_all_nonempty_suffixes xs = map reverse (get_all_nonempty_prefixes (reverse xs));;

get_all_nonempty_suffixes [1; 2; 3; 4; 0; 5];;
get_all_nonempty_suffixes ["Maddi"; "Hannah"; "Josh"; "Martha"]

let list_length xs =
  let rec list_length_tr xs acc =
    match xs with
      | [] -> acc
      | _::t -> list_length_tr t acc+1
  in list_length_tr xs 0;;

(* TODO: !!!!!DONE!!!!!Write 2 test cases to test list_length thoroughly. *)
list_length [1; 2; 3; 4; 5; 6; 7; 8; 9];;
list_length [1; 2; 3; 4];;

let take n xs =
  let rec take_tr n xs acc =
    if n <= 0 then
      acc
    else
      match xs with
        | h::t -> take_tr (n-1) t (acc@[h])
  in take_tr n xs [];;

(* TODO: !!!!!DONE!!!!!Write 4 test cases to test take thoroughly, including one that crashes.  Comment out that test case. *)
take 3 [1; 2; 3; 4; 5; 6];;
take 2 ["Maddison"; "Hannah"; "Josh"];;
(*take 5 [1; 2; 3];;*)
take 4 [1; 2; 3; 4; 5; 6; 7; 8];;

let rec drop n xs =
  if n <= 0 then
    xs
  else
    match xs with
      | _::t -> drop (n-1) t;;
	  
(* TODO: !!!!!DONE!!!!!Write 4 test cases to test drop thoroughly, including one that crashes.  Comment out that test case. *)

drop 3 [1; 2; 3; 4; 5; 6];;
drop 2 ["Maddison"; "Hannah"; "Josh"];;
(*drop 5 [1; 2; 3];;*)
drop 4 [1; 2; 3; 4; 5; 6; 7; 8];;

(* TODO: !!!!!DONE!!!!! Write comments describing how sublist can crash. *)
(* Sublist can crash by giving j values that are greater than the length of the string.*)
let sublist i j xs = take (j - i + 1) (drop (i - 1) xs);;

let all_two_ways xs =
  let rec all_two_ways_numbered xs nxs i acc =
    if i = nxs+1 then
      acc
    else
      all_two_ways_numbered xs nxs (i+1) (acc@[(take i xs), (drop i xs)])
  in all_two_ways_numbered xs (list_length xs) 0 [];;


(* TODO: !!!!!DONE!!!!!Write 2 test cases to test all_two_ways thoroughly. *)

all_two_ways["M"; "a"; "d"; "d"; "i"; "s"; "o"; "n"];;
all_two_ways[1; 2; 3; 4; 5; 6; 7; 8];;

let tuple_3 x (y, z) = (x, y, z);;

let two_way_to_three_ways (xs, ys) =
  let two_way_ys = all_two_ways ys in
    map (tuple_3 xs) two_way_ys;;
	
(* TODO: !!!!!DONE!!!!! Write two test cases to test two_way_to_three_ways thoroughly. *)

two_way_to_three_ways(["M"; "a"; "d"; "d"; "i"; "s"; "o"; "n"], ["H"; "a"; "n"; "n"; "a"; "h"]);;
two_way_to_three_ways([1; 2; 3; 4], [5; 4; 3; 2; 1]);;

(* TODO: !!!!!DONE!!!!!  Write a comment to explain what my_fold_left does. *)
(* It takes an operation, then a char, or list, or int, etc. (as long as it works with the operation), and a list or nested list, then it takes the char/list/int supplied and applies the operation to it and the first item in the list, then applies the operation with each subsequent item, building upon the previous operation. For example, if you pass (-) 1 [4; 3] it will look like this -> ((1 - 4) - 3) = -6 *)
let rec my_fold_left f id xs =
  match xs with
    | [] -> id
    | h::t -> my_fold_left f (f id h) t;;

let flatten xs = my_fold_left (@) [] xs;;

let all_three_ways xs = flatten (map two_way_to_three_ways (all_two_ways xs));;

(* TODO: !!!!!DONE!!!!!Write a test case to test all_three_ways thoroughly. *)
all_three_ways [1; 2; 3; 4; 5];;
all_three_ways ["Maddison"; "Hannah"; "Josh"; "Martha"];;

let position x the_list =
  let rec accpos i y the_list =
    match the_list with
      | [] -> -1
      | z::t -> if y = z then i else (accpos (i+1) y t)
  in (accpos 0 x the_list);;

let contains the_list x = (position x the_list) >= 0;;

(* TODO: !!!!!DONE!!!!! Write a test case to test contains thoroughly. *)
contains [1; 2; 3; 4] 4;;
contains ['a'; 'b'; 'c'] 'd';;

let all_contained_in the_list xs = my_fold_left (&&) true (map (contains the_list) xs);;

(* TODO: !!!!!DONE!!!!! Write three test cases to test all_contained_in_the_list thoroughly. *)

all_contained_in [1; 2; 3; 4] [1; 2];;
all_contained_in [1; 2; 3; 4] [1; 2; 5];;
all_contained_in ['a'; 'b'; 'c'; 'd'] ['a'; 'b'];;

let has_terminal_suffix terminals (u, w, v) =
  all_contained_in terminals v;;

let abcd_terminals = ['a';'b';'c';'d'];;

let aabcdd_form = explode "aabcdd";;

(* TODO: !!!!!DONE!!!!! Complete the below function and uncomment it and the test case below it. You need to break the sentential form into three pieces. The call to has_terminal_suffix is correct. It is an example of a partially applied function, or curried function. *)
let get_candidates sentential_form terminals = filter (has_terminal_suffix terminals) (all_three_ways sentential_form);;

get_candidates aabcdd_form abcd_terminals;;

(* TODO: !!!!!DONE!!!!! Complete (and uncomment) the below function. *)
let rec explode_all_RHS productions =
  match productions with
    | [] -> []
    | head::tail ->
        match head with
          | (lhs, rhs) ->
              (lhs, (explode rhs))::(explode_all_RHS tail);;

let amplus1bmcndnplus1_rules = [('S', "TU"); ('T', "aTb"); ('T', "a");
                                ('U', "cUd"); ('U', "d")];;

let batoi_rules = [('S',"Sa"); ('S', "b")];;

let aplusbpluscplus_rules = [('S', "Sc"); ('S', "Tc"); ('T', "Tb");
                             ('T', "Ub"); ('U', "Ua"); ('U', "a")];;

(* TODO: !!!!!DONE!!!!! Uncomment the below commented code. *)
explode_all_RHS amplus1bmcndnplus1_rules;;

explode_all_RHS batoi_rules;;

explode_all_RHS aplusbpluscplus_rules;;

(* TODO: !!!!!DONE!!!!!Complete (and uncomment) the below recursive function. *)
let rec are_lists_equal list1 list2 =
  match (list1, list2) with
    | ([],[]) -> true
    | (h1::t1, h2::t2) ->
        (h1 == h2) && (t1 == t2)
    | _ -> true;;

(* TODO: Complete (and uncomment) the below recursive function which produces a
   list of lefthand sides of all rules having w as a righthand side for the
   sentential form uwv. *)
let rec find_all_LHS rules (u, w, v) =
  match rules with
    | [] -> []
    | (lhs, rhs)::tail ->
        if (are_lists_equal rhs (u, w, v)) then
          lhs::(find_all_LHS tail (u, w, v))
        else
          ??? ???vgi[[[;;

let not_empty list = not (list = []);;

(* TODO: Uncomment the below commented code.
   let all_LHS rules candidates = map (find_all_LHS (explode_all_RHS rules)) candidates;;

   all_LHS amplus1bmcndnplus1_rules (get_candidates aabcdd_form abcd_terminals);; *)

(* This function produces sentential form uAv from uwv, assuming A -> w is a rule. *)
let reduce big_A (u, w, v) = (u, big_A, v);;

let rec zip f xs ys =
  match (xs, ys) with
    | ([], []) -> []
    | (a::ta, b::tb) -> (f a b)::(zip f ta tb)
    | (_, _) -> [];;

my_fold_left (+) 0 (zip ( * ) [1;2;3] [4;5;6]);;
(* TODO: !!!!!DONE!!!!! What vector operation is performed in the above call to my_fold_left? 
   Vector addition*)

(* This function produces a single list from the three list components of the tuple parameter. *)
let detuple_3 (x, y, z) = x@y@z;;

let is_valid_LHS (u, big_A, v) = not_empty big_A;;

(* TODO: Uncomment the below commented code. *) 
(* let reduce_all terminals rules sentential_form =
   let candidates = get_candidates sentential_form terminals in
   map detuple_3 (filter is_valid_LHS (zip reduce (all_LHS rules candidates) candidates));; *)

(* TODO: In the above reduce_all function, how many functions are explicitly called when reduce_all is called?
   There are seven functions  explicitly called*)
		 
(* TODO: Uncomment the below commented code. *)
(* let aabcdd_reduced_forms = reduce_all abcd_terminals amplus1bmcndnplus1_rules aabcdd_form;; *)

(* TODO: Complete (and uncomment) the below recursive algorithm which implements the breadth-first
   bottom-up parsing algorithm done in class. The explored parameter is a list containing
   all sentential forms checked so far. *)
(* let rec bottom_up_parse p terminals rules start_symbol explored queue =
   match ??? with
   | ??? -> (p^" is invalid", explored, queue)
   | sentential_form::tail ->
   if (??? [???] ???) then
   (p^" is valid", explored@[[???]], tail)
   else
   let next_level = (reduce_all ??? ??? ???) in
   ??? ??? ??? ??? ??? (explored@[???]) (tail@???);; *)

(* TODO: Uncomment the below code for testing when you have finished bottom_up_parse. *)
(* let parse p terminals rules start_symbol = bottom_up_parse p terminals rules start_symbol [] [explode p];;

   let parse_yes_no_only p terminals rules start_symbol =
   let (result,_,_) = parse p terminals rules start_symbol in
   result;;

   let num_forms_explored p terminals rules start_symbol =
   let (_,explored,_) = parse p terminals rules start_symbol in
   list_length explored;;

   parse "aabcdd" abcd_terminals amplus1bmcndnplus1_rules 'S';;

   parse_yes_no_only "aabcdd" abcd_terminals amplus1bmcndnplus1_rules 'S';;

   num_forms_explored "aabcdd" abcd_terminals amplus1bmcndnplus1_rules 'S';;

   parse "aaabccddd" abcd_terminals amplus1bmcndnplus1_rules 'S';;

   parse_yes_no_only "aaabccddd" abcd_terminals amplus1bmcndnplus1_rules 'S';;

   num_forms_explored "aaabccddd" abcd_terminals amplus1bmcndnplus1_rules 'S';;

   let rec dup_tr s n acc = if n <= 0 then acc else dup_tr s (n-1) (s^acc);;

   let dup s n = dup_tr s n "";;

   let ba100 = "b"^(dup "a" 100);;

   let ab_terminals = ['a'; 'b'];;

   parse ba100 ab_terminals batoi_rules 'S';;

   parse_yes_no_only ba100 ab_terminals batoi_rules 'S';;

   num_forms_explored ba100 ab_terminals batoi_rules 'S';;

   let ba100b = ba100^"b";;

   parse ba100b ab_terminals batoi_rules 'S';;

   parse_yes_no_only ba100b ab_terminals batoi_rules 'S';;

   num_forms_explored ba100b ab_terminals batoi_rules 'S';;

   let abc_terminals = ['a'; 'b'; 'c'];;

   let aabbc = "aabbc";;

   parse aabbc abc_terminals aplusbpluscplus_rules 'S';;

   parse_yes_no_only aabbc abc_terminals aplusbpluscplus_rules 'S';;

   num_forms_explored aabbc abc_terminals aplusbpluscplus_rules 'S';;

   let aabbcb = "aabbcb";;

   parse aabbcb abc_terminals aplusbpluscplus_rules 'S';;

   parse_yes_no_only aabbcb abc_terminals aplusbpluscplus_rules 'S';;

   num_forms_explored aabbcb abc_terminals aplusbpluscplus_rules 'S';;

*)

(* TODO: the last one. Yay!!!!!
   Develop a left-recursive grammar that generates {f}+, where f represents
   your first name.  It will have two rules.  Test it for f^2 by calling parse,
   parse_yes_no_only, and num_forms_explored.  Do this for each team member. *)
































